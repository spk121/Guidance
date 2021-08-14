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
#include <gio/gunixinputstream.h>
#include <gio/gunixoutputstream.h>
#include <glib.h>
#include <libguile.h>
#include <stdlib.h>

static SCM   update_thread_info (SCM self_var);
static SCM   update_trap_info (SCM self_var, SCM trap_cur);
static SCM   update_frame_info (SCM self_var, SCM frame);
static SCM   update_environment_info (SCM self_var, SCM env_vec);
static void *after_gc_handler (void *hook_data, void *fn_data, void *data);
static void *after_sweep_handler (void *hook_data, void *fn_data, void *data);

/* This GObject structure holds the interface between Guile and Gtk.
 */
struct _GdnLisp
{
  GObject parent;
  SCM     self;     /* A Guile pointer to this struct */
  SCM     self_var; /* aka `gdn-self`. The Guile variable that holds
                 * a pointer to this struct */

  SCM original_input_port;  /* Stores Guile's default `current-input-port` */
  SCM original_output_port; /* Stores Guile's default `current-output-port` */
  SCM original_error_port;  /* Stores Guile's default `current-error-port` */
  GPollableInputStream
      *input_stream;    /* Receives data from Guile's output port */
  SCM  output_port;     /* A substitute `current-output-port` */
  SCM  output_port_var; /* A Guile variable that holds output_port */
  GPollableInputStream
      *input_error_stream; /* Receives data from Guile's error ports */
  SCM  error_port;         /* A substitute `current-error-port` */
  SCM  error_port_var;     /* A Guile variable that hold the error port */
  GPollableInputStream
      *input_prompt_stream; /* Receives data from the Guile prompt port */
  SCM  prompt_port;         /* A new port for use with REPL prompts */
  SCM  prompt_port_var; /* aka `gdn-prompt-port`.  A Guile variable that holds
                           the REPL promp port. */
  GPollableOutputStream *output_stream; /* Sends data to Guile input port */
  SCM                    input_port;    /* A substitute `current-input-port` */
  SCM input_port_var; /* A Guile variable that hold the input port */

  int    response; /* Actually an enum of type `GdnReplCommand` */
  void * response_data;
  GCond  response_condition; /* Used in signalling operator responses */
  GMutex response_mutex;     /* Used in signalling operator responses */

  GListStore *frames;         /* Holds the last-computed backtrace as a list of `GdnFrameInfo` */
  GListStore *modules;        /* Holds the current list of modules as a list of `GdnModuleInfo` */
  GListStore *traps;          /* Holds the current list of traps as a list of `GdnTrapInfo` */
  GListStore *environment;    /* Holds the last-computed environment info as a list of
                            * `GdnEnvironmentInfo` */
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
static GPollableInputStream * unix_pty_input_stream_new (void);
static GPollableOutputStream *unix_pty_output_stream_new (void);
static SCM         port_from_unix_output_stream (GUnixOutputStream *steam);
static SCM         port_from_unix_input_stream (GUnixInputStream *steam);
static SCM         guile_interpreter (void *data);
static GEnumValue *categorize_file (GFile *file);
static SCM         get_trap_response (SCM self_ptr);

static SCM exit_handler (SCM self);
static SCM load_handler (SCM filename, SCM self);
static SCM module_defined_handler (SCM module, SCM self);
/////////////////////////////////////////////////////////////////

static void
gdn_lisp_class_init (GdnLispClass *klass)
{
  GBytes *contents;

  signals[SIGNAL_AFTER_GC] =
      g_signal_newv ("after-gc", G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS, NULL,
                     NULL, NULL, NULL,
                     G_TYPE_NONE, 0, NULL);

  signals[SIGNAL_AFTER_SWEEP] =
      g_signal_newv ("after-sweep", G_TYPE_FROM_CLASS (klass),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS, NULL,
                     NULL, NULL, NULL,
                     G_TYPE_NONE, 0, NULL);

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
   * but, use these special C procedures. These 'inner' functions
   * will receive the `gdn-self` pointer as their first argument. */

/* ISO C forbids converting a function to non-function pointer. That's a
 * bug in Guile. */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
  scm_c_define_gsubr ("gdn-update-thread-info", 1, 0, 0, update_thread_info);
  scm_c_define_gsubr ("gdn-update-trap-info", 2, 0, 0, update_trap_info);
  scm_c_define_gsubr ("gdn-update-frame-info", 2, 0, 0, update_frame_info);
  scm_c_define_gsubr ("gdn-update-environment-info", 2, 0, 0, update_environment_info);
  scm_c_define_gsubr ("gdn-exit-handler", 1, 0, 0, exit_handler);
  scm_c_define_gsubr ("gdn-load-handler", 2, 0, 0, load_handler);
  scm_c_define_gsubr ("gdn-module-defined-handler", 2, 0, 0, module_defined_handler);
  scm_c_define_gsubr ("gdn-get-trap-response", 1, 0, 0, get_trap_response);
#pragma GCC diagnostic pop
  scm_c_export ("gdn-update-thread-info",
                "gdn-update-environment-info",
                "gdn-exit-handler",
                "gdn-load-handler",
                "gdn-module-defined-handler",
                "gdn-trap-handler-inner",
                NULL);
  gui_thread = scm_current_thread ();

  // Loading this library sets up the GTK->Guile port mapping and defines our spawn functions.
  contents = g_resource_lookup_data (guidance_get_resource (),
                                     "/com/lonelycactus/Guidance/scm/lib.scm",
                                     G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
  scm_c_eval_string (g_bytes_get_data (contents, NULL));
  g_bytes_unref (contents);
  contents = g_resource_lookup_data (
      guidance_get_resource (), "/com/lonelycactus/Guidance/scm/guidance.scm",
      G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
  scm_c_eval_string (g_bytes_get_data (contents, NULL));
  g_bytes_unref (contents);

  run_repl_func = scm_variable_ref (scm_c_lookup ("gdn-run-repl"));
  run_argv_func = scm_variable_ref (scm_c_lookup ("gdn-run-argv"));
  run_trap_enable_func = scm_variable_ref (scm_c_lookup ("gdn-run-trap-enable"));
  run_trap_disable_func = scm_variable_ref (scm_c_lookup ("gdn-run-trap-disable"));
}

/* This initializes an instance of the lisp interpreter */
static void
gdn_lisp_init (GdnLisp *self)
{
  /* For some callbacks, we need to be able to access self's pointer
   * from within Guile. */
  self->self = scm_from_pointer ((void *) self, NULL);
  self->self_var = scm_c_define ("gdn-self", self->self);

  /* Create all the ports that bridge the worlds of GTK and Guile */
  self->original_input_port = scm_current_input_port ();
  self->output_stream = unix_pty_output_stream_new ();
  self->input_port =
      port_from_unix_output_stream (G_UNIX_OUTPUT_STREAM (self->output_stream));
  self->input_port_var = scm_c_define ("gdn-input-port", self->input_port);
  scm_set_current_input_port (self->input_port);

  self->original_output_port = scm_current_output_port ();
  self->input_stream = unix_pty_input_stream_new ();
  self->output_port =
      port_from_unix_input_stream (G_UNIX_INPUT_STREAM (self->input_stream));
  self->output_port_var = scm_c_define ("gdn-output-port", self->output_port);
  scm_set_current_output_port (self->output_port);

  self->original_error_port = scm_current_error_port ();
  self->input_error_stream = unix_pty_input_stream_new ();
  self->error_port = port_from_unix_input_stream (
      G_UNIX_INPUT_STREAM (self->input_error_stream));
  self->error_port_var = scm_c_define ("gdn-error-port", self->error_port);
  scm_set_current_error_port (self->error_port);

  /* Note that we are combining Guile's error and warning ports. */
  scm_set_current_warning_port (self->error_port);

  /* This is special port just to receive strings such as
   * `scheme@(guile-user)>` from the REPL. */
  self->input_prompt_stream = unix_pty_input_stream_new ();
  self->prompt_port = port_from_unix_input_stream (
      G_UNIX_INPUT_STREAM (self->input_prompt_stream));
  self->prompt_port_var = scm_c_define ("gdn-prompt-port", self->prompt_port);

  /* To transmit and receive operator command asynchronously, we use
   * atomic integers and condition variables */
  g_atomic_int_set (&(self->response), GDN_LISP_COMMAND_UNKNOWN);
  g_cond_init (&(self->response_condition));
  g_mutex_init (&(self->response_mutex));

  /* The list stores all require special member types. */
  self->frames = g_list_store_new (GDN_FRAME_INFO_TYPE);
  self->modules = g_list_store_new (GDN_MODULE_INFO_TYPE);
  self->traps = g_list_store_new (GDN_TRAP_INFO_TYPE);
  self->environment = g_list_store_new (GDN_ENVIRONMENT_INFO_TYPE);
  self->threads = g_list_store_new (GDN_THREAD_INFO_TYPE);

  /* A couple of the garbage collection hooks use the c_hook interface. */
  scm_c_hook_add (&scm_after_gc_c_hook, after_gc_handler, self, 0);
  scm_c_hook_add (&scm_after_sweep_c_hook, after_sweep_handler, self, 0);

  /* Finally, launch the REPL in its own thread. */
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
spawn_repl (G_GNUC_UNUSED void *data)
{
  scm_call_0 (run_repl_func);
  return SCM_UNSPECIFIED;
}

void
gdn_lisp_spawn_repl_thread (GdnLisp *self)
{
  SCM thrd = scm_spawn_thread (spawn_repl, NULL, NULL, NULL);
  /* FIXME: use scm_set_thread_cleanup_x to handle the exit of the repl thread. */
  self->default_thread = thrd;
}

static SCM
spawn_argv (void *data)
{
  SCM args = GPOINTER_TO_SCM (data);
  scm_call_1 (run_argv_func, args);
  return SCM_UNSPECIFIED;
}

void
gdn_lisp_spawn_argv_thread (GdnLisp *self, char **argv, gboolean break_on_entry)
{
  SCM args = SCM_EOL;
  while (*argv != NULL)
    {
      args = scm_cons (scm_from_locale_string (*argv), args);
      argv++;
    }
  args = scm_reverse (args);

  if (break_on_entry)
    scm_call_0 (run_trap_enable_func);
  else
    scm_call_0 (run_trap_disable_func);
  SCM thrd = scm_spawn_thread (spawn_argv, SCM_TO_GPOINTER (args), NULL, NULL);
  /* FIXME: use scm_set_thread_cleanup_x to handle the exit of the repl thread.
   */
  self->default_thread = thrd;
}

void
gdn_lisp_spawn_args_thread (GdnLisp *   self,
                            const char *args,
                            gboolean    break_on_entry)
{
  if (break_on_entry)
    scm_call_0 (run_trap_enable_func);
  else
    scm_call_0 (run_trap_disable_func);

  SCM thrd = scm_spawn_thread (spawn_argv, GPOINTER_TO_SCM (args), NULL, NULL);
  /* FIXME: use scm_set_thread_cleanup_x to handle the exit of the repl thread. */
  self->default_thread = thrd;
}

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
 * This procedure creates a new input stream backed by a unlocked PTY file descriptor.
 */
static GPollableInputStream *
unix_pty_input_stream_new (void)
{
  int fd = getpt ();
  if (fd == -1)
    g_error ("Could not create pseudoterminal device");
  if (grantpt (fd) < 0 || unlockpt (fd) < 0)
    g_error ("Could not unlock pseudoterminal FD #%d", fd);
  return G_POLLABLE_INPUT_STREAM (g_unix_input_stream_new (fd, TRUE));
}

/*
 * This procedure creates a new output stream backed by a unlocked PTY file descriptor.
 */

static GPollableOutputStream *
unix_pty_output_stream_new (void)
{
  int fd = getpt ();
  if (fd == -1)
    g_error ("Could not create pseudoterminal device");
  if (grantpt (fd) < 0 || unlockpt (fd) < 0)
    g_error ("Could not unlock pseudoterminal FD #%d", fd);
  return G_POLLABLE_OUTPUT_STREAM (g_unix_output_stream_new (fd, TRUE));
}

/*
 * This procedure creates a write-only Guile port paired with an input stream.
 */
static SCM
port_from_unix_input_stream (GUnixInputStream *stream)
{
  char *name;
  int   master_fd;
  int   slave_fd;
  SCM   mode;

  master_fd = g_unix_input_stream_get_fd (stream);
  name = ptsname (master_fd);
  if (name == NULL)
    g_error ("Could not acquire slave pseudoterminal device for FD #%d", master_fd);
  slave_fd = open (name, O_WRONLY);
  mode = scm_from_utf8_string ("w0");
  return scm_fdopen (scm_from_int (slave_fd), mode);
}

/*
 * This procedure creates a read-only Guile port paired with an output stream.
 */
static SCM
port_from_unix_output_stream (GUnixOutputStream *stream)
{
  char *name;
  int   master_fd;
  int   slave_fd;
  SCM   mode;

  master_fd = g_unix_output_stream_get_fd (stream);
  name = ptsname (master_fd);
  if (name == NULL)
    g_error ("Could not acquire slave pseudoterminal device for FD #%d", master_fd);
  /* FIXME: do I need to set a terminal mode w/ tcsetattr here?
   * If I do, it is the slave file descriptor that needs to be adjusted. */
  slave_fd = open (name, O_RDONLY);
  mode = scm_from_utf8_string ("r0");
  return scm_fdopen (scm_from_int (slave_fd), mode);
}

#if 0
static GEnumValue *
categorize_file(GFile *file)
{
    GdnFileCategory category = GDN_FILE_CATEGORY_UNKNOWN;

    char *library_dir = scm_to_utf8_string(scm_sys_library_dir());
    char *package_data_dir = scm_to_utf8_string(scm_sys_package_data_dir());
    char *site_dir = scm_to_utf8_string(scm_sys_site_dir());
    char *global_site_dir = scm_to_utf8_string(scm_c_eval_string("(%global-site-dir)"));
    char *filename = g_file_get_path(file);

    if (strncmp(library_dir, filename, strlen(library_dir)) == 0) {
        category = GDN_FILE_CATEGORY_LIBRARY;
    }
    else if (strncmp(package_data_dir, filename, strlen(package_data_dir)) == 0) {
        category = GDN_FILE_CATEGORY_PACKAGE_DATA;
    }
    else if (strncmp(site_dir, filename, strlen(site_dir)) == 0) {
        category = GDN_FILE_CATEGORY_SITE;
    }
    else if (strncmp(global_site_dir, filename, strlen(global_site_dir))) {
        category = GDN_FILE_CATEGORY_GLOBAL_SITE;
    }
    else {
        /* FIXME: should actually check that this filename is in the path. */
        category = GDN_FILE_CATEGORY_OTHER;
    }

    free(library_dir);
    free(package_data_dir);
    free(site_dir);
    free(global_site_dir);
    free(filename);

    return g_enum_get_value(GDN_FILE_CATEGORY_TYPE, category);
}
#endif

/*
 * This callback is used in the gdn-load-hook as part of a %load-hook
 * handler.
 */
static SCM
load_handler (SCM filename, SCM self_ptr)
{
  GdnLisp *self;
  char *   str;
  // GFile *     file;
  // GListStore *store;
  // GEnumValue *category;

  self = GDN_LISP (scm_to_pointer (self_ptr));

  str = scm_to_utf8_string (filename);
  g_debug ("loaded %s", str);
  // file = g_file_new_for_path (str);
  free (str);

  // category = categorize_file(file);

  // gtk_list_store_append(self->modules, &iter);
  // gtk_list_store_set(self->modules, &iter, 0, category, 1, file, -1);

  // FIXME: is it worth creating a "load" signal, or should we just rely on
  // the signals that occur when the list store is updated?
  // g_object_unref (category);
  return SCM_UNSPECIFIED;
}

/*
 * This callback is used in the gdn-module-defined-hook, which is
 * called on each new module by the module-defined-hook.
 */
static SCM
module_defined_handler (SCM module_info, SCM self_ptr)
{
  GdnLisp *self;

  self = GDN_LISP (scm_to_pointer (self_ptr));
  gdn_module_info_store_append (self->modules, module_info);
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
exit_handler (SCM self_var)
{
  GdnLisp *self;
  self = GDN_LISP (scm_to_pointer (self_var));
  g_signal_emit (self, signals[SIGNAL_EXIT], 0);
  return SCM_UNSPECIFIED;
}

static SCM
update_thread_info (SCM self_var)
{
  GdnLisp *self = scm_to_pointer (self_var);
  gdn_thread_info_store_update (self->threads);
  return SCM_UNSPECIFIED;
}

static SCM
update_trap_info (SCM self_var, SCM trap_cur)
{
  GdnLisp *self = scm_to_pointer (self_var);
  gdn_trap_info_store_update (self->traps, trap_cur);
  return SCM_UNSPECIFIED;
}

static SCM
update_frame_info (SCM self_var, SCM frames)
{
  GdnLisp *self = scm_to_pointer (self_var);
  gdn_frame_info_store_update (self->traps, frames);
  return SCM_UNSPECIFIED;
}

static SCM
update_environment_info (SCM self_var, SCM env_vec)
{
  GdnLisp *self = scm_to_pointer (self_var);
  gdn_environment_info_store_update (self->environment, env_vec);
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
get_trap_response (SCM self_ptr)
{
  GdnLisp *self = GDN_LISP (scm_to_pointer (self_ptr));
  SCM      response;

  g_mutex_lock (&(self->response_mutex));
  while (self->response != GDN_LISP_COMMAND_UNKNOWN)
    g_cond_wait (&(self->response_condition), &(self->response_mutex));

  response = response_data_to_scm (self);
  clear_response_data (self);
  g_mutex_unlock (&(self->response_mutex));

  return response;
}

GPollableInputStream *
gdn_lisp_get_input_stream (GdnLisp *lisp)
{
  return lisp->input_stream;
}

GPollableInputStream *
gdn_lisp_get_input_error_stream (GdnLisp *lisp)
{
  return lisp->input_error_stream;
}

GPollableInputStream *
gdn_lisp_get_input_prompt_stream (GdnLisp *lisp)
{
  return lisp->input_prompt_stream;
}

GOutputStream *
gdn_lisp_get_output_stream (GdnLisp *lisp)
{
  return lisp->output_stream;
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


/*
 * A thunk that launches a Guile REPL using the prompt port
 */
static SCM
guile_interpreter(void *data)
{
    GBytes *contents;
    SCM repl_prompt_port = GPOINTER_TO_SCM(data);

    /* Needed by the REPL. */
    scm_c_define("%gdn-prompt-port", repl_prompt_port);

    /* For the %load-hook callback. We make the closure here because
     * making it in C is hard. */
    scm_c_define_gsubr("%gdn-load-hook-inner", 2, 0, 0, gdn_app_win_scm_load_hook);
    scm_c_eval_string("(define (%gdn-load-hook fname)"
                      "  (%gdn-load-hook-inner fname %file-list-store))");

    contents = g_resource_lookup_data(guidance_get_resource(),
                                      "/com/lonelycactus/guidance/gdn-repl.scm",
                                      G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
    scm_c_eval_string(g_bytes_get_data(contents, NULL));
    g_bytes_unref(contents);
    return SCM_UNSPECIFIED;
}

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
