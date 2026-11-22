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

#include <gtk/gtk.h>
#include <libguile.h>

G_BEGIN_DECLS

#define SCM_TO_GPOINTER(x) ((gpointer) (scm_t_bits *) (SCM_UNPACK (x)))
#define GPOINTER_TO_SCM(p) (SCM_PACK ((scm_t_bits) p))

/*
 * Indicates an operator request to the lisp interpreter.  These are
 * delivered to the Scheme side as the symbols listed in (gdn repl).
 */
typedef enum
{
  GDN_LISP_COMMAND_UNKNOWN = 0,
  GDN_LISP_COMMAND_STEP_INTO,
  GDN_LISP_COMMAND_STEP_INTO_INSTRUCTION,
  GDN_LISP_COMMAND_STEP,
  GDN_LISP_COMMAND_STEP_INSTRUCTION,
  GDN_LISP_COMMAND_STEP_OUT,
  GDN_LISP_COMMAND_RUN,
  GDN_LISP_COMMAND_CONTINUE,
  GDN_LISP_COMMAND_STOP,
  GDN_LISP_COMMAND_EVAL,
  GDN_LISP_COMMAND_WAKE,
  GDN_LISP_N_COMMANDS
} GdnLispCommand;

typedef struct _GdnLispUserInput
{
  GdnLispCommand cmd;
  void *         data;
} GdnLispUserInput;

#define GDN_LISP_TYPE (gdn_lisp_get_type ())
G_DECLARE_FINAL_TYPE (GdnLisp, gdn_lisp, GDN, LISP, GObject)

GdnLisp *gdn_lisp_new (void);

/* Starts the Guidance REPL in a new Guile thread. */
void gdn_lisp_run (GdnLisp *self);

SCM  gdn_lisp_get_default_thread (GdnLisp *lisp);
int  gdn_lisp_get_input_fd (GdnLisp *self);
int  gdn_lisp_get_input_error_fd (GdnLisp *self);
int  gdn_lisp_get_input_prompt_fd (GdnLisp *self);
int  gdn_lisp_get_output_fd (GdnLisp *self);
SCM  gdn_lisp_to_scm (GdnLisp *self);
char **gdn_lisp_get_paths (void);

/* GTK THREAD: Delivers an operator action to the REPL, which is
 * waiting for one in error, trap, or REPL mode. */
void gdn_lisp_set_user_response (GdnLisp *      self,
                                 GdnLispCommand cmd,
                                 const void *   data);

/* GTK THREAD: Asks the REPL to add a trap at a call to PROC.  If the
 * program is RUNNING, the request is delivered asynchronously;
 * otherwise it is handled while the REPL waits for input. */
void gdn_lisp_add_proc_trap (GdnLisp *self, SCM proc, gboolean running);

/* GTK THREAD: Asks the REPL to stop the running program. */
void gdn_lisp_stop_async (GdnLisp *self);

G_END_DECLS
