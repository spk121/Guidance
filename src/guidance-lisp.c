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
#include "guidance-environment-view.h"
#include "guidance-frame-info.h"
#include "guidance-module-info.h"
#include "guidance-resources.h"
#include "guidance-thread-view.h"
#include "guidance-trap-info.h"
#include "guidance-trap-view.h"
#include <fcntl.h>
#include <gio/gio.h>
#include <glib.h>
#include <libguile.h>
#include <stdlib.h>

#include "guidance-backtrace-view.h"
#include "guidance-source-view.h"

/* Avoiding header recursion */
extern void gdn_application_window_guile_init (void);

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

  GAsyncQueue *user_input_queue;

  SCM         default_thread; /* The Guile representation of this interpreter's main thread */
};

G_DEFINE_TYPE (GdnLisp, gdn_lisp, G_TYPE_OBJECT)

enum
{
  SIGNAL_AFTER_GC = 0,
  SIGNAL_AFTER_SWEEP,
  SIGNAL_BREAK,
  N_SIGNALS
};

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static unsigned signals[N_SIGNALS];
static SCM      gui_thread;
static SCM      scm_lisp_type;
static SCM      add_trap_proc;
static SCM      trap_thunk_store[9], trap_func_store[9];
static int      trap_thunk_index = 1;

static SCM step_into_instruction_sym;
static SCM step_into_sym;
static SCM step_instruction_sym;
static SCM step_sym;
static SCM step_out_sym;
static SCM run_sym;
static SCM continue_sym;
static SCM stop_sym;
static SCM eval_sym;

static int         unix_pty_input_fd_new (void);
static int         unix_pty_output_fd_new (void);
static SCM         port_from_unix_output_fd (int fd);
static SCM         port_from_unix_input_fd (int fd);

static SCM  response_data_to_scm (GdnLispUserInput *input);
static SCM  scm_load_handler (SCM filename);
static SCM  spawn_top_repl (G_GNUC_UNUSED void *data);
static SCM  spawn_handler (void *data, SCM key, SCM args);
static void gdn_lisp_guile_init (void);
static SCM  scm_get_user_input (SCM s_self);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_gui_guile_init (void *user_data)
{
  gdn_lisp_guile_init ();
  gdn_application_window_guile_init ();
  gdn_backtrace_view_guile_init ();
  gdn_environment_view_guile_init ();
  gdn_module_info_guile_init ();
  gdn_source_view_guile_init ();
  gdn_thread_view_guile_init ();
  gdn_trap_info_guile_init ();
  gdn_trap_view_guile_init ();
}

static void
gdn_lisp_class_init (GdnLispClass *klass)
{

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

  /* The rest of the hooks and handlers are connected in Scheme,
   * but, use these special C procedures. */

/* ISO C forbids converting a function to non-function pointer. That's a
 * bug in Guile. */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#pragma GCC diagnostic pop
  scm_c_export ("%gdn-update-trap-info", NULL);
  gui_thread = scm_current_thread ();

  /* Not positive that turning off declarative modules is the
   * right thing, but let's try it. */
  scm_c_eval_string ("(user-modules-declarative? #f)");

  /* Everything in (gdn lib) should not require special gdn
   * primitives */
  GBytes *contents;
  contents = g_resource_lookup_data (guidance_get_resource (),
                                     "/com/lonelycactus/Guidance/gdn/lib.scm",
                                     G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
  scm_c_eval_string (g_bytes_get_data (contents, NULL));
  g_bytes_unref (contents);

  /* Provide GUI-related primitives */
  SCM module = scm_c_define_module ("gdn gui", gdn_gui_guile_init, NULL);
}

static void
gdn_ports_guile_init (void *user_data)
{
  GdnLisp *self = user_data;
  self->input_port_var = scm_c_define ("%gdn-input-port", self->input_port);
  self->output_port_var = scm_c_define ("%gdn-output-port", self->output_port);
  self->error_port_var = scm_c_define ("%gdn-error-port", self->error_port);
  self->prompt_port_var = scm_c_define ("%gdn-prompt-port", self->prompt_port);
  scm_c_export ("%gdn-input-port", "%gdn-output-port", "%gdn-error-port",
                "%gdn-prompt-port", NULL);
}

/* This initializes an instance of the lisp interpreter */
static void
gdn_lisp_init (GdnLisp *self)
{
  /* Create all the ports that bridge the worlds of GTK and Guile */
  self->original_input_port = scm_current_input_port ();
  self->output_fd = unix_pty_output_fd_new ();
  self->input_port = port_from_unix_output_fd (self->output_fd);
  scm_set_current_input_port (self->input_port);

  self->original_output_port = scm_current_output_port ();
  self->input_fd = unix_pty_input_fd_new ();
  self->output_port = port_from_unix_input_fd (self->input_fd);
  scm_set_current_output_port (self->output_port);

  self->original_error_port = scm_current_error_port ();
  self->input_error_fd = unix_pty_input_fd_new ();
  self->error_port = port_from_unix_input_fd (self->input_error_fd);
  scm_set_current_error_port (self->error_port);

  /* Note that we are combining Guile's error and warning ports. */
  scm_set_current_warning_port (self->error_port);

  /* This is special port just to receive strings such as
   * `scheme@(guile-user)>` from the REPL. */
  self->input_prompt_fd = unix_pty_input_fd_new ();
  self->prompt_port = port_from_unix_input_fd (self->input_prompt_fd);

  self->user_input_queue = g_async_queue_new ();

  /* A couple of the garbage collection hooks use the c_hook interface. */
  scm_c_hook_add (&scm_after_gc_c_hook, after_gc_handler, self, 0);
  scm_c_hook_add (&scm_after_sweep_c_hook, after_sweep_handler, self, 0);

  self->default_thread = SCM_BOOL_F;

  scm_c_define_module ("gdn ports", gdn_ports_guile_init, self);
}

////////////////////////////////////////////////////////////////
// METHODS
////////////////////////////////////////////////////////////////

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

void
gdn_lisp_run (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");
  GBytes *contents;
  contents = g_resource_lookup_data (guidance_get_resource (),
                                     "/com/lonelycactus/Guidance/gdn/repl.scm",
                                     G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
  scm_c_eval_string (g_bytes_get_data (contents, NULL));
  g_bytes_unref (contents);
  self->default_thread =
      scm_spawn_thread (spawn_top_repl, NULL, spawn_handler, NULL);
}

void
gdn_lisp_cancel_thread_async (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");
  g_assert (scm_is_true (scm_thread_p (self->default_thread)));

  scm_cancel_thread (self->default_thread);
}

void
gdn_lisp_exit_async (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");
  g_assert (scm_is_true (scm_thread_p (self->default_thread)));

  scm_system_async_mark_for_thread (scm_c_eval_string ("exit"),
                                    self->default_thread);
}

void
gdn_lisp_break_async (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");
  // This enqueues the trap handler in the current thread.
  // scm_system_async_mark_for_thread ();
}

gboolean
gdn_lisp_switch_thread (G_GNUC_UNUSED GdnLisp *lisp, G_GNUC_UNUSED int thd_idx)
{
  return TRUE;
}

SCM
gdn_lisp_get_default_thread (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");
  g_assert (scm_is_true (scm_thread_p (self->default_thread)));

  return self->default_thread;
}

char **
gdn_lisp_get_paths (void)
{
  SCM           path = scm_vector (scm_c_eval_string ("%load-path"));
  GStrvBuilder *builder = g_strv_builder_new ();
  for (size_t i = 0; i < scm_c_vector_length (path); i++)
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

int
gdn_lisp_get_input_fd (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");

  return self->input_fd;
}

int
gdn_lisp_get_input_error_fd (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");

  return self->input_error_fd;
}

int
gdn_lisp_get_input_prompt_fd (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");

  return self->input_prompt_fd;
}

int
gdn_lisp_get_output_fd (GdnLisp *self)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");

  return self->output_fd;
}

/* GTK THREAD: This function informs the Lisp intepreter that the user
 * has activated a debugging action.
 */
void
gdn_lisp_set_user_response (GdnLisp *self, GdnLispCommand cmd, const void *data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (self), ==, "GdnLisp");

  GdnLispUserInput *input = g_new0 (GdnLispUserInput, 1);
  input->cmd = cmd;
  if (cmd == GDN_LISP_COMMAND_EVAL)
    input->data = g_strdup (data);
  g_async_queue_push (self->user_input_queue, input);
}

/* GTK THREAD: This sends sends an async request to Guile to add a
 * trap at a given Guile procedure, at the next async opportunity. */
void
gdn_lisp_add_proc_trap_async (GdnLisp *self, SCM proc)
{
  trap_func_store[trap_thunk_index] = proc;
  scm_system_async_mark_for_thread (trap_thunk_store[trap_thunk_index],
                                    self->default_thread);
  trap_thunk_index++;
  if (trap_thunk_index > 8)
    trap_thunk_index = 1;
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static SCM
spawn_top_repl (G_GNUC_UNUSED void *data)
{
  // scm_c_eval_string ("((@ (ice-9 top-repl) top-repl))");
  scm_c_eval_string ("(use-modules (gdn lib) (gdn repl))");
  scm_c_eval_string ("(gdn-top-repl)");
  return SCM_UNSPECIFIED;
}

static SCM
spawn_handler (G_GNUC_UNUSED void *data, SCM key, SCM args)
{
  scm_simple_format (scm_current_output_port (),
                     scm_from_utf8_string ("Guile has exited"), SCM_EOL);
  scm_simple_format (scm_current_output_port (),
                     scm_from_utf8_string ("Key: ~S Args: ~S~%"),
                     scm_list_2 (key, args));
  return SCM_UNSPECIFIED;
}

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

static SCM
response_data_to_scm (GdnLispUserInput *input)
{
  SCM response;
  SCM response_data;

  response = SCM_BOOL_F;
  response_data = SCM_UNSPECIFIED;

  if (input->cmd == GDN_LISP_COMMAND_STEP_INTO_INSTRUCTION)
    response = step_into_instruction_sym;
  else if (input->cmd == GDN_LISP_COMMAND_STEP_INTO)
    response = step_into_sym;
  else if (input->cmd == GDN_LISP_COMMAND_STEP_INSTRUCTION)
    response = step_instruction_sym;
  else if (input->cmd == GDN_LISP_COMMAND_STEP)
    response = step_sym;
  else if (input->cmd == GDN_LISP_COMMAND_STEP_INSTRUCTION)
    response = step_instruction_sym;
  else if (input->cmd == GDN_LISP_COMMAND_STEP_OUT)
    response = step_out_sym;
  else if (input->cmd == GDN_LISP_COMMAND_RUN)
    response = run_sym;
  else if (input->cmd == GDN_LISP_COMMAND_CONTINUE)
    response = continue_sym;
  else if (input->cmd == GDN_LISP_COMMAND_STOP)
    response = stop_sym;
  else if (input->cmd == GDN_LISP_COMMAND_EVAL)
    {
      response = eval_sym;
      response_data = scm_from_utf8_string (input->data);
    }

  return scm_list_2 (response, response_data);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

SCM
gdn_lisp_to_scm (GdnLisp *self)
{
  SCM ret = scm_make_foreign_object_1 (scm_lisp_type, self);
  printf ("gdn_lisp_to_scm(%p,%p) -> %p\n", scm_lisp_type, self, ret);
  return ret;
}

/* GUILE THREAD: This function, which is running the Guile thread, is
 * a blocking wait for the Gtk thread to give the user's response. */
static SCM
scm_get_user_input (SCM s_self)
{
  scm_assert_foreign_object_type (scm_lisp_type, s_self);

  GdnLisp *self;
  SCM      s_input;

  self = scm_foreign_object_ref (s_self, 0);

  GdnLispUserInput *input;
  g_debug ("About to wait for input");
  input = g_async_queue_pop (self->user_input_queue);
  g_debug ("Received input");
  s_input = response_data_to_scm (input);
  g_free (input->data);
  g_free (input);

  return s_input;
}

/*
 * This callback is used in the gdn-load-hook as part of a %load-hook
 * handler.
 */
static SCM
scm_load_handler (SCM filename)
{
  g_assert (scm_is_string (filename));

  char *str;
  str = scm_to_utf8_string (filename);
  g_debug ("loaded %s", str);
  free (str);

  return SCM_UNSPECIFIED;
}
SCM
scm_trap_thunk_1 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[1] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[1]);
}

SCM
scm_trap_thunk_2 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[2] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[2]);
}

SCM
scm_trap_thunk_3 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[3] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[3]);
}

SCM
scm_trap_thunk_4 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[4] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[4]);
}

SCM
scm_trap_thunk_5 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[5] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[5]);
}

SCM
scm_trap_thunk_6 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[6] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[6]);
}

SCM
scm_trap_thunk_7 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[7] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[7]);
}

SCM
scm_trap_thunk_8 (void)
{
  g_assert (add_trap_proc != NULL);
  g_assert (trap_func_store[8] != NULL);
  return scm_call_1 (add_trap_proc, trap_func_store[8]);
}

void
gdn_lisp_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-lisp");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_lisp_type = scm_make_foreign_object_type (name, slots, NULL);
  printf ("scm_lisp_type %p\n", scm_lisp_type);

  step_into_instruction_sym = scm_from_utf8_symbol ("step-into-instruction");
  step_into_sym = scm_from_utf8_symbol ("step-into");
  step_instruction_sym = scm_from_utf8_symbol ("step-instruction");
  step_sym = scm_from_utf8_symbol ("step");
  step_out_sym = scm_from_utf8_symbol ("step-out");
  run_sym = scm_from_utf8_symbol ("run");
  run_sym = scm_from_utf8_symbol ("run");
  continue_sym = scm_from_utf8_symbol ("continue");
  stop_sym = scm_from_utf8_symbol ("stop");
  eval_sym = scm_from_utf8_symbol ("eval");

  scm_c_define_gsubr ("gdn-get-user-input", 1, 0, 0,
                      (scm_t_subr) scm_get_user_input);
  scm_c_define_gsubr ("gdn-load-handler", 1, 0, 0,
                      (scm_t_subr) scm_load_handler);
  trap_thunk_store[1] =
      scm_c_define_gsubr ("trap-thunk-1", 0, 0, 0, scm_trap_thunk_1);
  trap_thunk_store[2] =
      scm_c_define_gsubr ("trap-thunk-2", 0, 0, 0, scm_trap_thunk_2);
  trap_thunk_store[3] =
      scm_c_define_gsubr ("trap-thunk-3", 0, 0, 0, scm_trap_thunk_3);
  trap_thunk_store[4] =
      scm_c_define_gsubr ("trap-thunk-4", 0, 0, 0, scm_trap_thunk_4);
  trap_thunk_store[5] =
      scm_c_define_gsubr ("trap-thunk-5", 0, 0, 0, scm_trap_thunk_5);
  trap_thunk_store[6] =
      scm_c_define_gsubr ("trap-thunk-6", 0, 0, 0, scm_trap_thunk_6);
  trap_thunk_store[7] =
      scm_c_define_gsubr ("trap-thunk-7", 0, 0, 0, scm_trap_thunk_7);
  trap_thunk_store[8] =
      scm_c_define_gsubr ("trap-thunk-8", 0, 0, 0, scm_trap_thunk_8);
  add_trap_proc =
      scm_c_public_ref ("system vm trap-state", "add-trap-at-procedure-call!");

  scm_c_export ("gdn-get-user-input", NULL);
}
