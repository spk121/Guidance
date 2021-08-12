/* guidance-lisp.h
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

#pragma once

#include <gio/gunixinputstream.h>
#include <gio/gunixoutputstream.h>
#include <glib-object.h>

G_BEGIN_DECLS

/*
 * Indicates an operator request to the lisp interpreter during a
 * trap or break.
 */
typedef enum
{
  GDN_LISP_COMMAND_UNKNOWN = 0,
  GDN_LISP_COMMAND_STEP,
  GDN_LISP_COMMAND_STEP_INSTRUCTION,
  GDN_LISP_COMMAND_STEP_OUT,
  GDN_LISP_COMMAND_NEXT,
  GDN_LISP_COMMAND_NEXT_INSTRUCTION,
  GDN_LISP_COMMAND_RUN,
  GDN_LISP_COMMAND_RUN_DEBUG,
  GDN_LISP_COMMAND_CONTINUE,
  GDN_LISP_COMMAND_STOP,
  GDN_LISP_COMMAND_RESTART,
  GDN_LISP_N_COMMANDS
} GdnLispCommand;

#define GDN_LISP_TYPE (gdn_lisp_get_type ())
G_DECLARE_FINAL_TYPE (GdnLisp, gdn_lisp, GDN, LISP, GObject);

GdnLisp *gdn_lisp_new (void);
void     gdn_lisp_spawn_repl_thread (GdnLisp *lisp);
void     gdn_lisp_spawn_argv_thread (GdnLisp *lisp,
                                     char **  argv,
                                     gboolean break_on_entry);

/*
 * Spawns a new thread by processing a list of strings as if they were
 * command-line parameters.  Note that the first element is assumed to
 * be an argument and not the name of the command as in argv[0].
 *
 * Depending on the value of the `repl` flag, if the processing
 * completes executing without terminating the thread, a REPL may be
 * initiated in that module.
 *
 * @param lisp The object pointer.
 * @param argv A `GList` of strings.
 * @param repl If TRUE, a REPL will be executed in this thread after the
 *             command-line parameters have finished processing.
 */
void gdn_lisp_spawn_argv_thread (GdnLisp *lisp, GList *argv, gboolean repl);

/*
 * This procedure initiates an asynchronous cancellation of the default
 * thread.  It enqueues a Guile async mark to call `cancel-thread` on
 * the default thread.
 *
 * @param lisp The object pointer.
 */
void gdn_lisp_cancel_thread (GdnLisp *lisp);

/*
 * This procedure attempts to set the current thread to the n-th
 * thread in the thread store.  Not all threads in the thread store
 * can be used interactively, so this function may fail. On failure,
 * the current thread is not changed.
 *
 * @param lisp The object pointer.
 * @param n The index of the thread to attempt to set current.
 * @return It returns TRUE on success and FALSE on failure.
 */
gboolean gdn_lisp_switch_thread (GdnLisp *lisp, int thd_idx);

/*
 * This procedure initiates an asyncrhonous exit of the Guile
 * interpreter.  It enqueues a Guile async mark to call `exit` on the
 * default thread.
 *
 * @param lisp The object pointer.
 */
void gdn_lisp_exit (GdnLisp *lisp);

/*
 * This procedure initiates and asynchronous interruption in the
 * execution of the default threads.  It enqueues a Guile async mark
 * to interrupt the Guile thread.
 *
 * When the async is executed, the Lisp interpreter will interrupt, as
 * if it hit a trap, and await an event from the operator.
 *
 * @param lisp The object pointer.
 */
void gdn_lisp_break (GdnLisp *lisp);

/*
 * This procedure is called to indicate a command from the operator.
 * This command is one of step-over, step-into, return, or continue.
 * It should only be called when the Lisp interpreter's REPL thread is
 * awaiting a response, due to a break or a trap.
 *
 * It should only be called from the GTK main thread.
 *
 * @param lisp The object pointer.
 * @param command One of the step-over, step-into, return, or continue commands
 */
void gdn_lisp_continue (GdnLisp *lisp, GdnLispCommand command);

/*
 * This procedrue returns the input stream that receives from the
 * `current-output-port` of the Lisp interpreter.
 */
GUnixInputStream *gdn_lisp_get_input_stream (GdnLisp *lisp);

/*
 * Returns the input stream that receives from the
 * `current-error-port` of the Lisp interpreter.
 */
GUnixInputStream *gdn_lisp_get_input_error_stream (GdnLisp *lisp);

/*
 * Returns the input stream that receives from the special prompt port
 * of the Lisp interpreter.  The prompt port receives the prompt
 * characters that are printed just before operator input is
 * requested, such as `scheme@(guile-user)>`
 */
GUnixInputStream *gdn_lisp_get_input_prompt_stream (GdnLisp *lisp);

/*
 * Returns the output stream that transmits to the `current-input-port`
 * of the Lisp interpreter.
 */
// GUnixOutputStream *gdn_lisp_get_output_stream(GdnLisp *lisp);

/*
 * Hooks a func to input from the repl for a given main context.
 * Returns the source id of this GSource in the main context
 */
// Looks like
// pollable_input_stream
// call func to return GSource
// g_source_set_priority()
// g_source_set_callback() The callback is GPollableSourceFunc
// id = g_source_attach(source, NULL)
// return i
int gdn_lisp_set_input_callback (GdnLisp *            lisp,
                                 GMainContext *       ctx,
                                 GPollableSourceFunc *func,
                                 gpointer             data);

/*
 * Returns the store that holds environment info.
 * @param lisp The object pointer.
 * @return A GListStore of GdnEnvironmentInfo.
 */
GListStore *gdn_lisp_get_environment (GdnLisp *lisp);

/*
 * Requests an update of the environment info store.
 * @param lisp The object pointer.
 */
void gdn_lisp_update_environment (GdnLisp *lisp);

/*
 * Returns the store that holds Guile thread info.
 * @param lisp The object pointer.
 * @return A GListStore of GdnThreadInfo.
 */
GListStore *gdn_lisp_get_threads (GdnLisp *lisp);

/*
 * Requests an update of the Guile thread info store.
 * @param lisp The object pointer.
 */
void gdn_lisp_update_threads (GdnLisp *lisp);

/*
 * Returns the store that holds the frames from last stored backtrace.
 * @param lisp The object pointer.
 * @return Some sort of GListStore or GtkTreeListStore? of
 *         a GdnFrameInfo
 */
GListStore *gdn_lisp_get_frames (GdnLisp *lisp);

/*
 * Returns the store that hods the currently loaded modules.
 * @param lisp The object pointer.
 * @return A GListStore of a GdnModuleInfo.
 */
GListStore *gdn_lisp_get_modules (GdnLisp *lisp);

/*
 * Returns the store that holds the current traps.
 * @param lisp The object pointer.
 * @return A GListStore of GdnTrapInfo elements.
 */
GListStore *gdn_lisp_get_traps (GdnLisp *lisp);

void gdn_lisp_disable_trap (GdnLisp *lisp, int id);
void gdn_lisp_enable_trap (GdnLisp *lisp, int id);
void gdn_lisp_delete_trap (GdnLisp *lisp, int id);
int
    gdn_lisp_create_trap_at_procedure (GdnLisp *lisp, GFile *file, GString *name);
int gdn_lisp_create_trap_at_line (GdnLisp *lisp, GFile *file, int line);

G_END_DECLS
