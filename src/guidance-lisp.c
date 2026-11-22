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
#include "guidance-backtrace-view.h"
#include "guidance-environment-view.h"
#include "guidance-module-view.h"
#include "guidance-resources.h"
#include "guidance-source-view.h"
#include "guidance-thread-view.h"
#include "guidance-trap-info.h"
#include "guidance-trap-view.h"
#include <fcntl.h>
#include <gio/gio.h>
#include <glib.h>
#include <libguile.h>
#include <stdlib.h>

/* Avoiding header recursion */
extern void gdn_application_window_guile_init (void);

/* This GObject structure holds the interface between Guile and Gtk.
 *
 * The REPL runs in its own Guile thread, DEFAULT_THREAD.  Its standard
 * ports are replaced by pseudoterminals that the terminal view reads
 * and writes.  The GTK thread sends it operator actions through
 * USER_INPUT_QUEUE, and other requests, such as adding a trap,
 * through REQUEST_QUEUE.
 */
struct _GdnLisp
{
  GObject parent;
  SCM original_input_port;  /* Stores Guile's default `current-input-port` */
  SCM original_output_port; /* Stores Guile's default `current-output-port` */
  SCM original_error_port;  /* Stores Guile's default `current-error-port` */
  int input_fd;
  SCM output_port;     /* A substitute `current-output-port` */
  SCM output_port_var; /* A Guile variable that holds output_port */
  int input_error_fd;  /* Receives data from Guile's error ports */
  SCM error_port;      /* A substitute `current-error-port` */
  SCM error_port_var;  /* A Guile variable that hold the error port */
  int input_prompt_fd; /* Receives data from the Guile prompt port */
  SCM prompt_port;     /* A new port for use with REPL prompts */
  SCM prompt_port_var; /* aka `%gdn-prompt-port`.  A Guile variable that
                          holds the REPL prompt port. */
  int output_fd;       /* Sends data to Guile input port */
  SCM input_port;      /* A substitute `current-input-port` */
  SCM input_port_var;  /* A Guile variable that hold the input port */

  GAsyncQueue *user_input_queue; /* of GdnLispUserInput */
  GAsyncQueue *request_queue;    /* of GC-protected SCM requests */
  gint         interrupt;        /* Atomic GdnLispInterrupt */
  gint         running;          /* Atomic: a program is running */

  SCM default_thread; /* The Guile thread running the REPL */
};

G_DEFINE_TYPE (GdnLisp, gdn_lisp, G_TYPE_OBJECT)

/* What the operator asked a running program to do. */
typedef enum
{
  GDN_LISP_INTERRUPT_NONE = 0,
  GDN_LISP_INTERRUPT_STOP,
  GDN_LISP_INTERRUPT_PAUSE
} GdnLispInterrupt;

enum
{
  SIGNAL_AFTER_GC = 0,
  SIGNAL_AFTER_SWEEP,
  N_SIGNALS
};

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static unsigned signals[N_SIGNALS];
static SCM      scm_lisp_type;
static SCM      async_poll_proc = SCM_BOOL_F;
static SCM      stop_sym;
static SCM      pause_sym;

static SCM command_syms[GDN_LISP_N_COMMANDS];

static void *after_gc_handler (void *hook_data, void *fn_data, void *data);
static void *after_sweep_handler (void *hook_data, void *fn_data, void *data);

static int  unix_pty_fd_new (void);
static SCM  port_from_unix_input_fd (int fd);
static SCM  port_from_unix_output_fd (int fd);
static void eval_resource (const char *path, const char *file_name);
static void mark_async_poll (GdnLisp *self);

static SCM  response_data_to_scm (GdnLispUserInput *input);
static SCM  spawn_top_repl (void *data);
static SCM  spawn_handler (void *data, SCM key, SCM args);
static void gdn_lisp_guile_init (void);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_gui_guile_init (G_GNUC_UNUSED void *user_data)
{
  gdn_lisp_guile_init ();
  gdn_application_window_guile_init ();
  gdn_backtrace_view_guile_init ();
  gdn_environment_view_guile_init ();
  gdn_source_view_guile_init ();
  gdn_thread_view_guile_init ();
  gdn_trap_info_guile_init ();
  gdn_trap_view_guile_init ();
  gdn_module_view_guile_init ();
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

  /* Not positive that turning off declarative modules is the
   * right thing, but let's try it. */
  scm_c_eval_string ("(user-modules-declarative? #f)");

  /* Everything in (gdn lib) should not require special gdn
   * primitives */
  eval_resource ("/com/lonelycactus/Guidance/gdn/lib.scm", "gdn/lib.scm");

  /* Provide GUI-related primitives */
  scm_c_define_module ("gdn gui", gdn_gui_guile_init, NULL);
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
  self->output_fd = unix_pty_fd_new ();
  self->input_port = port_from_unix_output_fd (self->output_fd);
  scm_set_current_input_port (self->input_port);

  self->original_output_port = scm_current_output_port ();
  self->input_fd = unix_pty_fd_new ();
  self->output_port = port_from_unix_input_fd (self->input_fd);
  scm_set_current_output_port (self->output_port);

  self->original_error_port = scm_current_error_port ();
  self->input_error_fd = unix_pty_fd_new ();
  self->error_port = port_from_unix_input_fd (self->input_error_fd);
  scm_set_current_error_port (self->error_port);

  /* Note that we are combining Guile's error and warning ports. */
  scm_set_current_warning_port (self->error_port);

  /* This is special port just to receive strings such as
   * `repl>` from the REPL. */
  self->input_prompt_fd = unix_pty_fd_new ();
  self->prompt_port = port_from_unix_input_fd (self->input_prompt_fd);

  self->user_input_queue = g_async_queue_new ();
  self->request_queue = g_async_queue_new ();
  self->interrupt = GDN_LISP_INTERRUPT_NONE;
  self->running = FALSE;

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
 * The stdin, stdout, and stderr ports for that lisp interpreter are
 * mapped to pseudoterminals, so that we can communicate with the
 * interpreter via file descriptors.
 *
 * Call gdn_lisp_run to start the interpreter's thread.
 */
GdnLisp *
gdn_lisp_new (void)
{
  return (GdnLisp *) g_object_new (GDN_LISP_TYPE, NULL);
}

void
gdn_lisp_run (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));

  eval_resource ("/com/lonelycactus/Guidance/gdn/repl.scm", "gdn/repl.scm");
  self->default_thread =
      scm_spawn_thread (spawn_top_repl, NULL, spawn_handler, NULL);
}

SCM
gdn_lisp_get_default_thread (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));
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
  g_assert (GDN_IS_LISP (self));
  return self->input_fd;
}

int
gdn_lisp_get_input_error_fd (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));
  return self->input_error_fd;
}

int
gdn_lisp_get_input_prompt_fd (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));
  return self->input_prompt_fd;
}

int
gdn_lisp_get_output_fd (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));
  return self->output_fd;
}

/* GTK THREAD: This function informs the Lisp interpreter that the user
 * has activated a debugging action.
 */
void
gdn_lisp_set_user_response (GdnLisp *self, GdnLispCommand cmd, const void *data)
{
  g_assert (GDN_IS_LISP (self));
  g_assert (cmd > GDN_LISP_COMMAND_UNKNOWN && cmd < GDN_LISP_N_COMMANDS);

  GdnLispUserInput *input = g_new0 (GdnLispUserInput, 1);
  input->cmd = cmd;
  if (cmd == GDN_LISP_COMMAND_EVAL)
    input->data = g_strdup (data);
  g_async_queue_push (self->user_input_queue, input);
}

/* Removes the operator's actions (run, continue, stop, and the steps)
 * that are waiting for the REPL thread.  Expressions that the operator
 * entered are kept, and so are wake-ups, which stand for requests. */
void
gdn_lisp_discard_actions (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));

  GQueue            kept = G_QUEUE_INIT;
  GdnLispUserInput *input;

  g_async_queue_lock (self->user_input_queue);
  while ((input = g_async_queue_try_pop_unlocked (self->user_input_queue))
         != NULL)
    {
      if (input->cmd == GDN_LISP_COMMAND_EVAL
          || input->cmd == GDN_LISP_COMMAND_WAKE)
        g_queue_push_tail (&kept, input);
      else
        g_free (input);
    }
  while ((input = g_queue_pop_head (&kept)) != NULL)
    g_async_queue_push_unlocked (self->user_input_queue, input);
  g_async_queue_unlock (self->user_input_queue);
}

void
gdn_lisp_set_running (GdnLisp *self, gboolean running)
{
  g_atomic_int_set (&self->running, running);
}

gboolean
gdn_lisp_is_running (GdnLisp *self)
{
  return g_atomic_int_get (&self->running);
}

/* GTK THREAD: Queues a request for the REPL thread, which is a Scheme
 * list such as (delete-trap 3). */
static void
push_request (GdnLisp *self, SCM request)
{
  g_assert (GDN_IS_LISP (self));

  g_async_queue_push (self->request_queue,
                      SCM_TO_GPOINTER (scm_gc_protect_object (request)));
  if (gdn_lisp_is_running (self))
    /* The program is running, so the REPL isn't waiting for input.
     * Interrupt it. */
    mark_async_poll (self);
  else
    /* The REPL is waiting for input, or soon will be. */
    gdn_lisp_set_user_response (self, GDN_LISP_COMMAND_WAKE, NULL);
}

static SCM
sym (const char *name)
{
  return scm_from_utf8_symbol (name);
}

static SCM
str (const char *s)
{
  return scm_from_utf8_string (s);
}

void
gdn_lisp_request_proc_trap (GdnLisp *   self,
                            const char *module_name,
                            const char *symbol)
{
  push_request (self, scm_list_3 (sym ("add-proc-trap-by-name"),
                                  str (module_name), str (symbol)));
}

void
gdn_lisp_request_source_trap (GdnLisp *self, const char *file, int line)
{
  push_request (self, scm_list_3 (sym ("add-source-trap"), str (file),
                                  scm_from_int (line)));
}

void
gdn_lisp_request_trap_enabled (GdnLisp *self, int index, gboolean enable)
{
  push_request (self, scm_list_2 (sym (enable ? "enable-trap" : "disable-trap"),
                                  scm_from_int (index)));
}

void
gdn_lisp_request_trap_delete (GdnLisp *self, int index)
{
  push_request (self, scm_list_2 (sym ("delete-trap"), scm_from_int (index)));
}

void
gdn_lisp_request_module_procedures (GdnLisp *self, const char *module_name)
{
  push_request (self,
                scm_list_2 (sym ("module-procedures"), str (module_name)));
}

void
gdn_lisp_request_set_program (GdnLisp *self, const char *file)
{
  push_request (self, scm_list_2 (sym ("set-program"), str (file)));
}

void
gdn_lisp_request_select_frame (GdnLisp *self, int index)
{
  push_request (self, scm_list_2 (sym ("select-frame"), scm_from_int (index)));
}

void
gdn_lisp_stop_async (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));

  /* The flag guards against a late async interrupting a later program:
   * the REPL clears it whenever it starts running something.  Stop
   * overrides a pending pause. */
  g_atomic_int_set (&self->interrupt, GDN_LISP_INTERRUPT_STOP);
  mark_async_poll (self);
}

void
gdn_lisp_pause_async (GdnLisp *self)
{
  g_assert (GDN_IS_LISP (self));

  /* A pending stop is not downgraded to a pause. */
  g_atomic_int_compare_and_exchange (&self->interrupt,
                                     GDN_LISP_INTERRUPT_NONE,
                                     GDN_LISP_INTERRUPT_PAUSE);
  mark_async_poll (self);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static gboolean
emit_after_gc (gpointer user_data)
{
  g_signal_emit (user_data, signals[SIGNAL_AFTER_GC], 0);
  return G_SOURCE_REMOVE;
}

static gboolean
emit_after_sweep (gpointer user_data)
{
  g_signal_emit (user_data, signals[SIGNAL_AFTER_SWEEP], 0);
  return G_SOURCE_REMOVE;
}

/*
 * A scm_t_c_hook function that emits an "after-gc" signal.  Guile runs
 * it in whichever thread collected garbage, so the signal is emitted
 * from the GTK thread instead.
 */
static void *
after_gc_handler (G_GNUC_UNUSED void *hook_data,
                  void *              fn_data,
                  G_GNUC_UNUSED void *data)
{
  g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, emit_after_gc,
                   g_object_ref (fn_data), g_object_unref);
  return NULL;
}

/*
 * A scm_t_c_hook function that emits an "after-sweep" signal from the
 * GTK thread.
 */
static void *
after_sweep_handler (G_GNUC_UNUSED void *hook_data,
                     void *              fn_data,
                     G_GNUC_UNUSED void *data)
{
  g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, emit_after_sweep,
                   g_object_ref (fn_data), g_object_unref);
  return NULL;
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

/* Evaluates a Scheme file stored in the GResource bundle.  It is
 * compiled rather than interpreted: compiled procedures are faster,
 * and Guile's procedure traps can only tell compiled procedures
 * apart.
 *
 * FILE_NAME is recorded as the source file of its procedures, as if it
 * had been loaded from the load path.  The debugger recognizes
 * Guidance's own frames by it, to keep them out of backtraces and
 * stepping; see guile-internal-frame? in (gdn lib). */
static void
eval_resource (const char *path, const char *file_name)
{
  GBytes *     contents;
  gsize        len;
  const char * data;
  SCM          eval_string;

  contents = g_resource_lookup_data (guidance_get_resource (), path,
                                     G_RESOURCE_LOOKUP_FLAGS_NONE, NULL);
  g_assert_nonnull (contents);
  data = g_bytes_get_data (contents, &len);
  eval_string = scm_c_public_ref ("ice-9 eval-string", "eval-string");
  scm_call_5 (eval_string, scm_from_utf8_stringn (data, len),
              scm_from_utf8_keyword ("compile?"), SCM_BOOL_T,
              scm_from_utf8_keyword ("file"), scm_from_utf8_string (file_name));
  g_bytes_unref (contents);
}

/* Runs gdn-async-poll in the REPL thread at its next safe point. */
static void
mark_async_poll (GdnLisp *self)
{
  if (scm_is_false (async_poll_proc))
    async_poll_proc = scm_c_public_ref ("gdn repl", "gdn-async-poll");
  scm_system_async_mark_for_thread (async_poll_proc, self->default_thread);
}

static SCM
spawn_top_repl (G_GNUC_UNUSED void *data)
{
  /* Programs and expressions are evaluated in (guile-user), as in
   * Guile's own REPL. */
  scm_c_eval_string ("(set-current-module (resolve-module '(guile-user)))");
  scm_c_eval_string ("(use-modules (gdn lib) (gdn repl))");
  return scm_c_eval_string ("(gdn-top-repl)");
}

static SCM
spawn_handler (G_GNUC_UNUSED void *data, SCM key, SCM args)
{
  scm_simple_format (scm_current_output_port (),
                     scm_from_utf8_string ("Guile has exited.~%Key: ~S Args: ~S~%"),
                     scm_list_2 (key, args));
  return SCM_UNSPECIFIED;
}

/*
 * This procedure creates a new unlocked pseudoterminal and returns its
 * master file descriptor.
 */
static int
unix_pty_fd_new (void)
{
  int fd = posix_openpt (O_RDWR | O_NOCTTY);
  if (fd == -1)
    g_error ("Could not create pseudoterminal device");
  if (grantpt (fd) < 0 || unlockpt (fd) < 0)
    g_error ("Could not unlock pseudoterminal FD #%d", fd);
  return fd;
}

/* Guidance's text widgets expect UTF-8, whatever the locale. */
static SCM
set_port_utf8 (SCM port)
{
  scm_set_port_encoding_x (port, scm_from_utf8_string ("UTF-8"));
  return port;
}

/*
 * This procedure creates a write-only Guile port paired with an input fd.
 */
static SCM
port_from_unix_input_fd (int master_fd)
{
  char *name;
  int   slave_fd;

  name = ptsname (master_fd);
  if (name == NULL)
    g_error ("Could not acquire slave pseudoterminal device for FD #%d",
             master_fd);
  slave_fd = open (name, O_WRONLY | O_NOCTTY);
  if (slave_fd < 0)
    g_error ("Could not open pseudoterminal %s", name);
  return set_port_utf8 (
      scm_fdopen (scm_from_int (slave_fd), scm_from_utf8_string ("w0")));
}

/*
 * This procedure creates a read-only Guile port paired with an output fd.
 */
static SCM
port_from_unix_output_fd (int master_fd)
{
  char *name;
  int   slave_fd;

  name = ptsname (master_fd);
  if (name == NULL)
    g_error ("Could not acquire slave pseudoterminal device for FD #%d",
             master_fd);
  /* FIXME: do I need to set a terminal mode w/ tcsetattr here?
   * If I do, it is the slave file descriptor that needs to be adjusted. */
  slave_fd = open (name, O_RDONLY | O_NOCTTY);
  if (slave_fd < 0)
    g_error ("Could not open pseudoterminal %s", name);
  return set_port_utf8 (
      scm_fdopen (scm_from_int (slave_fd), scm_from_utf8_string ("r0")));
}

static SCM
response_data_to_scm (GdnLispUserInput *input)
{
  SCM response = SCM_BOOL_F;
  SCM response_data = SCM_BOOL_F;

  if (input->cmd > GDN_LISP_COMMAND_UNKNOWN
      && input->cmd < GDN_LISP_N_COMMANDS)
    response = command_syms[input->cmd];
  if (input->cmd == GDN_LISP_COMMAND_EVAL)
    response_data = scm_from_utf8_string (input->data);

  return scm_list_2 (response, response_data);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

SCM
gdn_lisp_to_scm (GdnLisp *self)
{
  return scm_make_foreign_object_1 (scm_lisp_type, self);
}

static void *
pop_user_input (void *queue)
{
  return g_async_queue_pop (queue);
}

/* GUILE THREAD: This function, which is running the Guile thread, is
 * a blocking wait for the Gtk thread to give the user's response.  It
 * leaves Guile mode while waiting, so that it doesn't hold up garbage
 * collection. */
static SCM
scm_get_user_input (SCM s_self)
{
  scm_assert_foreign_object_type (scm_lisp_type, s_self);

  GdnLisp *         self = scm_foreign_object_ref (s_self, 0);
  GdnLispUserInput *input;
  SCM               s_input;

  input = scm_without_guile (pop_user_input, self->user_input_queue);
  s_input = response_data_to_scm (input);
  g_free (input->data);
  g_free (input);

  return s_input;
}

/* GUILE THREAD: Returns the queued requests, oldest first. */
static SCM
scm_take_requests_x (SCM s_self)
{
  scm_assert_foreign_object_type (scm_lisp_type, s_self);

  GdnLisp *self = scm_foreign_object_ref (s_self, 0);
  SCM      requests = SCM_EOL;
  gpointer item;

  while ((item = g_async_queue_try_pop (self->request_queue)) != NULL)
    {
      SCM request = GPOINTER_TO_SCM (item);
      requests = scm_cons (request, requests);
      /* REQUESTS keeps it alive from here. */
      scm_gc_unprotect_object (request);
    }
  return scm_reverse_x (requests, SCM_EOL);
}

/* GUILE THREAD: Returns, once, the symbol stop or pause if the operator
 * asked to stop or pause the program, or #f. */
static SCM
scm_take_interrupt_request_x (SCM s_self)
{
  scm_assert_foreign_object_type (scm_lisp_type, s_self);

  GdnLisp *self = scm_foreign_object_ref (s_self, 0);
  switch (g_atomic_int_exchange (&self->interrupt, GDN_LISP_INTERRUPT_NONE))
    {
    case GDN_LISP_INTERRUPT_STOP:
      return stop_sym;
    case GDN_LISP_INTERRUPT_PAUSE:
      return pause_sym;
    default:
      return SCM_BOOL_F;
    }
}

static SCM
scm_clear_interrupt_request_x (SCM s_self)
{
  scm_assert_foreign_object_type (scm_lisp_type, s_self);

  GdnLisp *self = scm_foreign_object_ref (s_self, 0);
  g_atomic_int_set (&self->interrupt, GDN_LISP_INTERRUPT_NONE);
  return SCM_UNSPECIFIED;
}

void
gdn_lisp_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-lisp");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_lisp_type = scm_make_foreign_object_type (name, slots, NULL);

  command_syms[GDN_LISP_COMMAND_UNKNOWN] = SCM_BOOL_F;
  command_syms[GDN_LISP_COMMAND_STEP_INTO] = scm_from_utf8_symbol ("step-into");
  command_syms[GDN_LISP_COMMAND_STEP_INTO_INSTRUCTION] =
      scm_from_utf8_symbol ("step-into-instruction");
  command_syms[GDN_LISP_COMMAND_STEP] = scm_from_utf8_symbol ("step");
  command_syms[GDN_LISP_COMMAND_STEP_INSTRUCTION] =
      scm_from_utf8_symbol ("step-instruction");
  command_syms[GDN_LISP_COMMAND_STEP_OUT] = scm_from_utf8_symbol ("step-out");
  command_syms[GDN_LISP_COMMAND_RUN] = scm_from_utf8_symbol ("run");
  command_syms[GDN_LISP_COMMAND_CONTINUE] = scm_from_utf8_symbol ("continue");
  command_syms[GDN_LISP_COMMAND_STOP] = scm_from_utf8_symbol ("stop");
  command_syms[GDN_LISP_COMMAND_EVAL] = scm_from_utf8_symbol ("eval");
  command_syms[GDN_LISP_COMMAND_WAKE] = scm_from_utf8_symbol ("wake");
  stop_sym = scm_from_utf8_symbol ("stop");
  pause_sym = scm_from_utf8_symbol ("pause");

  scm_c_define_gsubr ("gdn-get-user-input", 1, 0, 0,
                      (scm_t_subr) scm_get_user_input);
  scm_c_define_gsubr ("gdn-take-requests!", 1, 0, 0,
                      (scm_t_subr) scm_take_requests_x);
  scm_c_define_gsubr ("gdn-take-interrupt-request!", 1, 0, 0,
                      (scm_t_subr) scm_take_interrupt_request_x);
  scm_c_define_gsubr ("gdn-clear-interrupt-request!", 1, 0, 0,
                      (scm_t_subr) scm_clear_interrupt_request_x);
  scm_c_export ("gdn-get-user-input", "gdn-take-requests!",
                "gdn-take-interrupt-request!", "gdn-clear-interrupt-request!",
                NULL);
}
