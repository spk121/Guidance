/* guidance-lisp.c
 *
 * Copyright 2021 Michael Gran
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#define _GNU_SOURCE
#include "guidance-lisp.h"
#include "guidance-environment-info.h"
#include "guidance-frame-info.h"
#include "guidance-module-info.h"
#include "guidance-resources.h"
#include "guidance-thread-info.h"
#include "guidance-trap-info.h"
#include <fcntl.h>
#include <gio/gio.h>
#include <glib.h>
#include <libguile.h>
#include <stdlib.h>

#include "guidance-backtrace-view.h"
#include "guidance-source-view.h"

static SCM   update_thread_info (void);
static SCM   update_trap_info (SCM trap_cur);
static SCM   update_environment_info (SCM env_vec);
static void *after_gc_handler (void *hook_data, void *fn_data, void *data);
static void *after_sweep_handler (void *hook_data, void *fn_data, void *data);

/* This GObject structure holds the interface between Guile and Gtk.
 */
struct _GdnLisp
{
  GObject parent;
  SCM original_input_port;  /* Stores Guile's default `current-input-port` */
  SCM original_output_port; /* Stores Guile's default `current-output-port` */
  SCM original_error_port;  /* Stores Guile's default `current-error-port` */
  int  input_fd;
  SCM  output_port;     /* A substitute `current-output-port` */
  SCM  output_port_var; /* A Guile variable that holds output_port */
  int  input_error_fd;  /* Receives data from Guile's error ports */
  SCM  error_port;         /* A substitute `current-error-port` */
  SCM  error_port_var;     /* A Guile variable that hold the error port */
  int  input_prompt_fd;    /* Receives data from the Guile prompt port */
  SCM  prompt_port;         /* A new port for use with REPL prompts */
  SCM  prompt_port_var; /* aka `%gdn-prompt-port`.  A Guile variable that holds
                           the REPL promp port. */
  int  output_fd;       /* Sends data to Guile input port */
  SCM  input_port;      /* A substitute `current-input-port` */
  SCM input_port_var; /* A Guile variable that hold the input port */

  int    response; /* Actually an enum of type `GdnReplCommand` */
  void * response_data;
  GCond  response_condition; /* Used in signalling operator responses */
  GMutex response_mutex;     /* Used in signalling operator responses */

  GListStore *modules;        /* Holds the current list of modules as a list of `GdnModuleInfo` */
  GListStore *traps;          /* Holds the current list of traps as a list of `GdnTrapInfo` */
  GtkTreeListModel *environment; /* Holds the last-computed environment info as
                                  * a list of `GdnEnvironmentInfo` */
  GListStore *threads;        /* Holds the last-computed list of active Guile threads. */

  SCM         default_thread; /* The Guile representation of this interpreter's main thread */
};

G_DEFINE_TYPE (GdnLisp, gdn_lisp, G_TYPE_OBJECT)

enum
{
  SIGNAL_AFTER_GC = 0,
  SIGNAL_AFTER_SWEEP,
  SIGNAL_BREAK,
  SIGNAL_EXIT,
  N_SIGNALS
};

static unsigned signals[N_SIGNALS];
static SCM      gui_thread;
static SCM      run_repl_func;
static SCM      run_argv_func;
static SCM      run_trap_enable_func;
static SCM      run_trap_disable_func;

////////////////////////////////////////////////////////////////
static int         unix_pty_input_fd_new (void);
static int         unix_pty_output_fd_new (void);
static SCM         port_from_unix_output_fd (int fd);
static SCM         port_from_unix_input_fd (int fd);
static SCM         get_trap_response (void);

static SCM exit_handler (void);
static SCM load_handler (SCM filename);
static SCM module_defined_handler (SCM module);
/////////////////////////////////////////////////////////////////

static void
gdn_lisp_class_init (GdnLispClass *klass)
{
  GBytes *contents;

  signals[SIGNAL_AFTER_GC] =
      g_signal_newv ("after-gc", G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, NULL, NULL, NULL,
                     NULL, G_TYPE_NONE, 0, NULL);

  signals[SIGNAL_AFTER_SWEEP] =
      g_signal_newv ("after-sweep", G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, NULL, NULL, NULL,
                     NULL, G_TYPE_NONE, 0, NULL);

  signals[SIGNAL_BREAK] =
      g_signal_newv ("break", G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS, NULL,
                     NULL, NULL, NULL,
                     G_TYPE_NONE, 0, NULL);

  signals[SIGNAL_EXIT] =
      g_signal_newv ("exit", G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS, NULL,
                     NULL, NULL, NULL,
                     G_TYPE_NONE, 0, NULL);

  /* The rest of the hooks and handlers are connected in Scheme,
   * but, use these special C procedures. */

/* ISO C forbids converting a function to non-function pointer. That's a
 * bug in Guile. */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
  scm_c_define_gsubr ("%gdn-update-thread-info", 0, 0, 0, update_thread_info);
  scm_c_define_gsubr ("%gdn-update-trap-info", 1, 0, 0, update_trap_info);
  scm_c_define_gsubr ("%gdn-update-environment-info", 1, 0, 0,
                      update_environment_info);
  scm_c_define_gsubr ("%gdn-exit-handler", 0, 0, 0, exit_handler);
  scm_c_define_gsubr ("%gdn-load-handler", 1, 0, 0, load_handler);
  scm_c_define_gsubr ("%gdn-module-defined-handler", 1, 0, 0,
                      module_defined_handler);
  scm_c_define_gsubr ("%gdn-get-trap-response", 0, 0, 0, get_trap_response);

#pragma GCC diagnostic pop
  scm_c_export ("%gdn-update-thread-info", "%gdn-update-trap-info",
                "%gdn-update-environment-info", "%gdn-exit-handler",
                "%gdn-load-handler", "%gdn-module-defined-handler", NULL);
  gui_thread = scm_current_thread ();

  // Loading this library sets up the GTK->Guile port mapping and defines our spawn functions.
  contents = g_resource_lookup_data (guidance_get_resource (),
                                     "/com/lonelycactus/Guidance/gdn/lib.scm",
                                     G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
  scm_c_eval_string (g_bytes_get_data (contents, NULL));
  g_bytes_unref (contents);

#if 0
  run_repl_func = scm_variable_ref (scm_c_lookup ("gdn-run-repl"));
  run_argv_func = scm_variable_ref (scm_c_lookup ("gdn-run-argv"));
  run_trap_enable_func = scm_variable_ref (scm_c_lookup ("gdn-run-trap-enable"));
  run_trap_disable_func = scm_variable_ref (scm_c_lookup ("gdn-run-trap-disable"));
#endif
  gdn_source_view_guile_init ();
  gdn_backtrace_view_guile_init ();
}

static GdnLisp *_self = NULL;

/* This initializes an instance of the lisp interpreter */
static void
gdn_lisp_init (GdnLisp *self)
{
  /* FIXME: this class is effectively singleton. Can multiple
   * instances work? */
  _self = self;

  /* Create all the ports that bridge the worlds of GTK and Guile */
  self->original_input_port = scm_current_input_port ();
  self->output_fd = unix_pty_output_fd_new ();
  self->input_port = port_from_unix_output_fd (self->output_fd);
  self->input_port_var = scm_c_define ("%gdn-input-port", self->input_port);
  scm_set_current_input_port (self->input_port);

  self->original_output_port = scm_current_output_port ();
  self->input_fd = unix_pty_input_fd_new ();
  self->output_port = port_from_unix_input_fd (self->input_fd);
  self->output_port_var = scm_c_define ("%gdn-output-port", self->output_port);
  scm_set_current_output_port (self->output_port);

  self->original_error_port = scm_current_error_port ();
  self->input_error_fd = unix_pty_input_fd_new ();
  self->error_port = port_from_unix_input_fd (self->input_error_fd);
  self->error_port_var = scm_c_define ("%gdn-error-port", self->error_port);
  scm_set_current_error_port (self->error_port);

  /* Note that we are combining Guile's error and warning ports. */
  scm_set_current_warning_port (self->error_port);

  /* This is special port just to receive strings such as
   * `scheme@(guile-user)>` from the REPL. */
  self->input_prompt_fd = unix_pty_input_fd_new ();
  self->prompt_port = port_from_unix_input_fd (self->input_prompt_fd);
  self->prompt_port_var = scm_c_define ("%gdn-prompt-port", self->prompt_port);

  /* To transmit and receive operator command asynchronously, we use
   * atomic integers and condition variables */
  g_atomic_int_set (&(self->response), GDN_LISP_COMMAND_UNKNOWN);
  g_cond_init (&(self->response_condition));
  g_mutex_init (&(self->response_mutex));

  /* The list stores all require special member types. */
  self->modules = g_list_store_new (GDN_MODULE_INFO_TYPE);
  self->traps = g_list_store_new (GDN_TRAP_INFO_TYPE);
  self->environment = gdn_environment_info_get_tree_model ();
  self->threads = g_list_store_new (GDN_THREAD_INFO_TYPE);

  /* A couple of the garbage collection hooks use the c_hook interface. */
  scm_c_hook_add (&scm_after_gc_c_hook, after_gc_handler, self, 0);
  scm_c_hook_add (&scm_after_sweep_c_hook, after_sweep_handler, self, 0);

  self->default_thread = SCM_BOOL_F;
}

/**
 * Creates a new Lisp interpreter.
 *
 * It spawns a new Lisp interpreter in a new thread.  The stdin,
 * stdout, and stderr ports for that lisp interpreter are mapped to
 * GUnixInputStream and GUnixOutputStream types, so that we can
 * communicate with the intepreter via pseudoterminal file
 * descriptors.
 *
 * You need to call one of the spawn methods to activate the interpreter.
 */
GdnLisp *
gdn_lisp_new (void)
{
  return (GdnLisp *) g_object_new (GDN_LISP_TYPE, NULL);
}

static SCM
spawn_top_repl (G_GNUC_UNUSED void *data)
{
  scm_c_eval_string ("((@ (ice-9 top-repl) top-repl))");
  return SCM_UNSPECIFIED;
}

void
gdn_lisp_spawn_repl_thread (GdnLisp *self)
{
  SCM thrd = scm_spawn_thread (spawn_top_repl, NULL, NULL, NULL);
  /* FIXME: use scm_set_thread_cleanup_x to handle the exit of the repl thread. */
  self->default_thread = thrd;
}

static SCM
spawn_shell (void *data)
{
  scm_shell (g_strv_length ((char **) data), (char **) data);
  return SCM_UNSPECIFIED;
}

SCM
spawn_handler (void *data, SCM key, SCM args)
{
  scm_simple_format (scm_current_output_port (),
                     scm_from_utf8_string ("Guile has exited"), SCM_EOL);
  scm_simple_format (scm_current_output_port (),
                     scm_from_utf8_string ("Key: ~S Args: ~S~%"),
                     scm_list_2 (key, args));
  return SCM_UNSPECIFIED;
}

void
gdn_lisp_spawn_argv_thread (GdnLisp *self, char **argv, gboolean break_on_entry)
{
  int argc = 0;

  /* In original mode, to mock up breaking on entry, we start off with
   * a regular top-level REPL.  The command-line arguments will be
   * processed separately, perhaps as a response to the "play"
   * button. */
  SCM thrd;
  // if (break_on_entry)
  //{
  self->default_thread =
      scm_spawn_thread (spawn_top_repl, NULL, spawn_handler, NULL);
  //}
  // else
  // thrd = scm_spawn_thread (spawn_shell, argv, spawn_handler, NULL);
  self->default_thread = thrd;
}

#if 0
void
gdn_lisp_spawn_args_thread (GdnLisp *   self,
                            const char *args,
                            gboolean    break_on_entry)
{
#if 0
  if (break_on_entry)
      //scm_catch(SCM_BOOL_T, run_trap_enable_func, SCM_BOOL_F);
      scm_call_0 (run_trap_enable_func);
  else
    scm_call_0 (run_trap_disable_func);
#endif
  scm_init_guile();
  SCM thrd = scm_spawn_thread (spawn_shell, GPOINTER_TO_SCM (args), NULL, NULL);
  /* FIXME: use scm_set_thread_cleanup_x to handle the exit of the repl thread. */
  self->default_thread = thrd;
}
#endif

void
gdn_lisp_cancel_thread (GdnLisp *self)
{
  scm_cancel_thread (self->default_thread);
}

void
gdn_lisp_exit (GdnLisp *self)
{
  scm_system_async_mark_for_thread (scm_c_eval_string ("exit"), self->default_thread);
}

void
gdn_lisp_break (G_GNUC_UNUSED GdnLisp *self)
{
  // This enqueues the trap handler in the current thread.
  // scm_system_async_mark_for_thread ();
}

////////////////////////////////////////////////////////////////

/*
 * This procedure creates a new input fd-backed by a unlocked PTY file
 * descriptor.
 */
static int
unix_pty_input_fd_new (void)
{
  int fd = getpt ();
  if (fd == -1)
    g_error ("Could not create pseudoterminal device");
  if (grantpt (fd) < 0 || unlockpt (fd) < 0)
    g_error ("Could not unlock pseudoterminal FD #%d", fd);
  return fd;
}

/*
 * This procedure creates a new output fd backed by a unlocked PTY file
 * descriptor.
 */

static int
unix_pty_output_fd_new (void)
{
  int fd = getpt ();
  if (fd == -1)
    g_error ("Could not create pseudoterminal device");
  if (grantpt (fd) < 0 || unlockpt (fd) < 0)
    g_error ("Could not unlock pseudoterminal FD #%d", fd);
  return fd;
}

/*
 * This procedure creates a write-only Guile port paired with an input fd.
 */
static SCM
port_from_unix_input_fd (int master_fd)
{
  char *name;
  int   slave_fd;
  SCM   mode;

  name = ptsname (master_fd);
  if (name == NULL)
    g_error ("Could not acquire slave pseudoterminal device for FD #%d", master_fd);
  slave_fd = open (name, O_WRONLY);
  mode = scm_from_utf8_string ("w0");
  return scm_fdopen (scm_from_int (slave_fd), mode);
}

/*
 * This procedure creates a read-only Guile port paired with an output fd.
 */
static SCM
port_from_unix_output_fd (int master_fd)
{
  char *name;
  int   slave_fd;
  SCM   mode;

  name = ptsname (master_fd);
  if (name == NULL)
    g_error ("Could not acquire slave pseudoterminal device for FD #%d", master_fd);
  /* FIXME: do I need to set a terminal mode w/ tcsetattr here?
   * If I do, it is the slave file descriptor that needs to be adjusted. */
  slave_fd = open (name, O_RDONLY);
  mode = scm_from_utf8_string ("r0");
  return scm_fdopen (scm_from_int (slave_fd), mode);
}

/*
 * This callback is used in the gdn-load-hook as part of a %load-hook
 * handler.
 */
static SCM
load_handler (SCM filename)
{
  g_assert (scm_is_string (filename));

  char *   str;
  str = scm_to_utf8_string (filename);
  g_debug ("loaded %s", str);
  free (str);

  return SCM_UNSPECIFIED;
}

/*
 * This callback is used in the gdn-module-defined-hook, which is
 * called on each new module by the module-defined-hook.
 */
static SCM
module_defined_handler (SCM module_info)
{
  g_assert (scm_is_vector (module_info));
  g_assert_cmpint (scm_c_vector_length (module_info), ==, 4);
  g_assert (scm_is_string (scm_c_vector_ref (module_info, 0)));
  g_assert (scm_is_string (scm_c_vector_ref (module_info, 1)));
  g_assert (scm_is_string (scm_c_vector_ref (module_info, 2)));
  g_assert (scm_is_symbol (scm_c_vector_ref (module_info, 3)));

  g_assert (_self != NULL);
  g_assert (_self->modules != NULL);

  gdn_module_info_store_append (_self->modules, module_info);
  return SCM_UNSPECIFIED;
}

/*
 * A scm_t_c_hook function that emits an "after-gc" signal
 */
static void *
after_gc_handler (G_GNUC_UNUSED void *hook_data,
                  void *              fn_data,
                  G_GNUC_UNUSED void *data)
{
  g_signal_emit (fn_data, signals[SIGNAL_AFTER_GC], 0);

  return NULL;
}

/*
 * A scm_t_c_hook function that emits an "after-sweep" signal
 */
static void *
after_sweep_handler (G_GNUC_UNUSED void *hook_data,
                     void *              fn_data,
                     G_GNUC_UNUSED void *data)
{
  g_signal_emit (fn_data, signals[SIGNAL_AFTER_SWEEP], 0);

  return NULL;
}

static SCM
exit_handler (void)
{
  g_assert (_self != NULL);
  g_signal_emit (_self, signals[SIGNAL_EXIT], 0);
  return SCM_UNSPECIFIED;
}

static SCM
update_thread_info (void)
{
  gdn_thread_info_store_update (_self->threads);
  return SCM_UNSPECIFIED;
}

static SCM
update_trap_info (SCM trap_cur)
{
  g_assert (scm_is_integer (trap_cur));
  gdn_trap_info_store_update (_self->traps, scm_to_int (trap_cur));
  return SCM_UNSPECIFIED;
}

static SCM
update_environment_info (SCM info)
{
  g_assert (scm_is_vector (info));
#ifndef G_DISABLE_ASSERT
  for (size_t i = 0; i < scm_c_vector_length (info); i++)
    {
      SCM entry = scm_c_vector_ref (info, i);
      g_assert (scm_is_vector (entry));
      g_assert_cmpint (scm_c_vector_length (entry), ==, 2);
      g_assert (scm_is_string (scm_c_vector_ref (entry, 0)));
      // Categories have keys, but, no values.
      // Each category must have at least one child.
      SCM children = scm_c_vector_ref (entry, 1);
      g_assert (scm_c_vector_length (children) > 0);
    }
#endif

  gdn_environment_info_update_all (info);
  return SCM_UNSPECIFIED;
}

static void
clear_response_data (GdnLisp *self)
{
  if (self->response == GDN_LISP_COMMAND_EVAL)
    g_free (self->response_data);
  self->response_data = NULL;
  self->response = GDN_LISP_COMMAND_UNKNOWN;
}

static void
set_response_data (GdnLisp *self, GdnLispCommand cmd, void *data)
{
  self->response = cmd;
  if (self->response == GDN_LISP_COMMAND_EVAL)
    self->response_data = g_strdup (data);
  else
    self->response_data = data;
}

static SCM
response_data_to_scm (GdnLisp *self)
{
  SCM step_into_instruction_sym =
      scm_from_utf8_symbol ("step-into-instruction");
  SCM step_into_sym = scm_from_utf8_symbol ("step-into");
  SCM step_instruction_sym = scm_from_utf8_symbol ("step-instruction");
  SCM step_sym = scm_from_utf8_symbol ("step");
  SCM step_out_sym = scm_from_utf8_symbol ("step-out");
  SCM continue_sym = scm_from_utf8_symbol ("continue");
  SCM eval_sym = scm_from_utf8_symbol ("eval");

  SCM response;
  SCM response_data;

  response_data = SCM_UNSPECIFIED;

  if (self->response == GDN_LISP_COMMAND_STEP_INTO_INSTRUCTION)
    response = step_into_instruction_sym;
  else if (self->response == GDN_LISP_COMMAND_STEP_INTO)
    response = step_into_sym;
  else if (self->response == GDN_LISP_COMMAND_STEP_INSTRUCTION)
    response = step_into_sym;
  else if (self->response == GDN_LISP_COMMAND_STEP)
    response = step_sym;
  else if (self->response == GDN_LISP_COMMAND_STEP_INSTRUCTION)
    response = step_instruction_sym;
  else if (self->response == GDN_LISP_COMMAND_STEP_OUT)
    response = step_out_sym;
  else if (self->response == GDN_LISP_COMMAND_CONTINUE)
    response = continue_sym;
  else if (self->response == GDN_LISP_COMMAND_EVAL)
    {
      response = eval_sym;
      response_data = scm_from_utf8_string (self->response_data);
    }
  return scm_list_2 (response, response_data);
}

/* This function, which is running the GTK main thread, informs the
 * Lisp intepreter that the user has activated a debugging action.
 */
void
gdn_lisp_trap_set_user_response (GdnLisp *self, GdnLispCommand cmd, void *data)
{
  g_mutex_lock (&(self->response_mutex));

  // If an old command didn't get processed, clear out the
  // associated data. */
  if (self->response != GDN_LISP_COMMAND_UNKNOWN)
    {
      g_warning ("Unprocessed debug user action %d", self->response);
      clear_response_data (self);
    }
  set_response_data (self, cmd, data);
  g_cond_signal (&(self->response_condition));
  g_mutex_unlock (&(self->response_mutex));
}

/* This function, which is running the Guile thread, is a blocking
 * wait for the Gtk thread to give the user's response. */
static SCM
get_trap_response (void)
{
  SCM      response;

  g_assert (_self != NULL);
  g_mutex_lock (&(_self->response_mutex));
  while (_self->response != GDN_LISP_COMMAND_UNKNOWN)
    g_cond_wait (&(_self->response_condition), &(_self->response_mutex));

  response = response_data_to_scm (_self);
  clear_response_data (_self);
  g_mutex_unlock (&(_self->response_mutex));

  return response;
}

int
gdn_lisp_get_input_fd (GdnLisp *lisp)
{
  return lisp->input_fd;
}

int
gdn_lisp_get_input_error_fd (GdnLisp *lisp)
{
  return lisp->input_error_fd;
}

int
gdn_lisp_get_input_prompt_fd (GdnLisp *lisp)
{
  return lisp->input_prompt_fd;
}

int
gdn_lisp_get_output_fd (GdnLisp *lisp)
{
  return lisp->output_fd;
}

#if 0

struct _GdnAppWindow
{
    GtkApplicationWindow parent;

    /* For the headerbar. */
    GtkWidget *gears;

    /* For the REPL widget. */
    GtkScrolledWindow *repl_scrolled_window;
    GtkTextView *repl_output;
    GtkLabel *repl_prompt;
    GtkEntry *repl_input;
    GList *history;
    GList *history_cur;
    GdnPty *pty;
    SCM gui_thread;
    SCM default_thread;

    /* For the explorer tabl */
    GtkListBox *filename_listbox;

    /* Styyyyle */
    GtkStyleProvider *style;

};

SCM
gdn_repl_spawn_thread(SCM prompt_port)
{
    SCM thrd = scm_spawn_thread(guile_interpreter, SCM_TO_GPOINTER(prompt_port), NULL, NULL);
    /* FIXME: use scm_set_thread_cleanup_x to handle the exit of the repl thread. */
    return thrd;
}


SCM
gdn_app_win_scm_load_hook(SCM s_filename)
{
    GFile *file;
    GdnCategorizedFile *fcat;
    GdnCategorizedFileCollection *fcoll;
    GtkTreeIter *iter;
    GString *short_name;
    GString *long_name;
    GString *path;
    GdnFileCategory category;
    GValue short_name_value = G_VALUE_INIT;
    GValue long_name_value = G_VALUE_INIT;
    GValue path_value = G_VALUE_INIT;
    GValue category_value = G_VALUE_INIT;

    short_name = g_string_new(NULL);
    long_name = g_string_new(NULL);
    path = g_string_new(NULL);

    file = gdn_gfile_new_from_scm(s_filename);
    category = gdn_categorize_file(file, short_name, long_name, path);

    g_value_init(&short_name_value, G_TYPE_STRING);
    g_value_init(&long_name_value, G_TYPE_STRING);
    g_value_init(&path_value, G_TYPE_STRING);
    g_value_init(&category_value, GDN_TYPE_CATEGORY);

    g_value_take_string(&short_name_value, g_string_free(short_name, FALSE));
    g_value_take_string(&long_name_value, g_string_free(long_name, FALSE));
    g_value_take_string(&path_name_value, g_string_free(path_name, FALSE));
    g_value_set_enum(&category_value, (int)category);

    /* Since GdnCategorizedFileStore is a GListStore, the "row-changed"
     *  and "row-added" signals should be used to update the view */
    g_list_store_append(fcol, iter);
    g_list_store_set_value(fcol, iter, GDN_CATEGORIZED_FILE_CATEGORY_COLUMN, gcategory_value);
    g_list_store_set_value(fcol, iter, GDN_CATEGORIZED_FILE_SHORT_NAME_COLUMN, short_name_value);
    g_list_store_set_value(fcol, iter, GDN_CATEGORIZED_FILE_LONG_NAME_COLUMN, long_name_value);
    g_list_store_set_value(fcol, iter, GDN_CATEGORIZED_FILE_PATH_COLUMN, path_value);

    g_value_unset(&short_name_value);
    g_value_unset(&long_name_value);
    g_value_unset(&path_name_value);
    g_value_unset(&category_value);

    return SCM_UNSPECIFIED;
}

GString *
gdn_gstring_new_from_scm(SCM s_str)
{
    char *c_str;
    GString *g_str;

    if (!scm_is_string(s_str))
        g_error("input is not a scheme string");
    c_str = scm_to_utf8_string(s_str);
    g_str = g_string_new(c_str);
    free(c_str);
    return g_str;
}

/*
 * Creates an SCM port connected to a C pty handle
 *
 * It takes a master pseudoterminal handle, makes sure it is unlocked,
 * creates a slave file descriptor, and then makes an SCM port for the
 * slave FD.
 */
static SCM
open_scm_port(int master_file_descriptor, int slave_access_mode)
{
    char *name;
    int slave_file_descriptor;
    SCM s_slave_file_descriptor;
    SCM s_access_mode;
    SCM slave_port;

    if (grantpt(master_file_descriptor) < 0 || unlockpt(master_file_descriptor) < 0)
        g_error("Could not unlock pseudoterminal FD #%d", master_file_descriptor);

    name = ptsname(master_file_descriptor);
    if (name == NULL)
        g_error("Could not acquire slave pseudoterminal device for FD #%d",
                master_file_descriptor);

    slave_file_descriptor = open(name, slave_access_mode);
    if (slave_file_descriptor == -1)
        g_error("Could not open slave file descriptor for pseudoterminal %s", name);

    /* FIXME: do I need to set a terminal mode w/ tcsetattr here?
     * If I do, it is the slave file descriptor that needs to be adjusted. */

    s_slave_file_descriptor = scm_from_int(slave_file_descriptor);
    if (slave_access_mode == O_WRONLY)
        s_access_mode = scm_from_utf8_string("w0");
    else if (slave_access_mode == O_RDONLY)
        s_access_mode = scm_from_utf8_string("r0");
    else
        g_error("Unknown access mode %d", slave_access_mode);

    slave_port = scm_fdopen(s_slave_file_descriptor, s_access_mode);
    return slave_port;
}

/*
 * Creates all the pty handles and SCM ports needed by the REPL
 *
 * Each of the file descriptors will be populated with a pseudoterminal handle.
 * Each of the SCM ports will read or write to the file descriptors.
 *
 */
static void
open_scm_ports(int *input_file_descriptor_master,
               int *output_file_descriptor_master,
               int *error_file_descriptor_master,
               int *prompt_file_descriptor_master,
               SCM *slave_input_scm_port,
               SCM *slave_output_scm_port, SCM *slave_error_scm_port, SCM *slave_prompt_scm_port)
{

    if ((*output_file_descriptor_master = getpt()) < 0)
        g_error("Could not create master pseudoterminal device for output");
    *slave_output_scm_port = open_scm_port(*output_file_descriptor_master, O_WRONLY);
    if ((*error_file_descriptor_master = getpt()) < 0)
        g_error("Could not create master pseudoterminal device for error");
    *slave_error_scm_port = open_scm_port(*error_file_descriptor_master, O_WRONLY);
    if ((*prompt_file_descriptor_master = getpt()) < 0)
        g_error("Could not create master pseudoterminal device for prompt");
    *slave_prompt_scm_port = open_scm_port(*prompt_file_descriptor_master, O_WRONLY);
    if ((*input_file_descriptor_master = getpt()) < 0)
        g_error("Could not create master pseudoterminal device for input");
    *slave_input_scm_port = open_scm_port(*input_file_descriptor_master, O_RDONLY);
}
#endif

GListStore *
gdn_lisp_get_threads (GdnLisp *self)
{
  return self->threads;
}

GListStore *
gdn_lisp_get_modules (GdnLisp *self)
{
  return self->modules;
}

GListStore *
gdn_lisp_get_environment (GdnLisp *self)
{
  return G_LIST_STORE (self->environment);
}

gboolean
gdn_lisp_switch_thread (GdnLisp *lisp, int thd_idx)
{
  return TRUE;
}

char **
gdn_lisp_get_paths (GdnLisp *lisp)
{
  SCM           path = scm_vector (scm_c_eval_string ("%load-path"));
  GStrvBuilder *builder = g_strv_builder_new ();
  for (int i = 0; i < scm_c_vector_length (path); i++)
    {
      char *str;
      str = scm_to_utf8_string (scm_c_vector_ref (path, i));
      g_strv_builder_add (builder, str);
      free (str);
    }
  char **strv;
  strv = g_strv_builder_end (builder);
  g_strv_builder_unref (builder);
  return strv;
}

void
gdm_lisp_scm_shell (void)
{
}
