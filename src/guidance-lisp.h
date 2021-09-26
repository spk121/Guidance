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
 * Indicates an operator request to the lisp interpreter during a
 * trap or break.
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
  GDN_LISP_COMMAND_RUN_DEBUG,
  GDN_LISP_COMMAND_CONTINUE,
  GDN_LISP_COMMAND_STOP,
  GDN_LISP_COMMAND_RESTART,
  GDN_LISP_COMMAND_EVAL,
  GDN_LISP_N_COMMANDS
} GdnLispCommand;

#define GDN_LISP_TYPE (gdn_lisp_get_type ())
G_DECLARE_FINAL_TYPE (GdnLisp, gdn_lisp, GDN, LISP, GObject)

GdnLisp *             gdn_lisp_new (void);
GdnLisp *             gdn_lisp_get_lisp (void);
void                  gdn_lisp_run (void);
void                  gdn_lisp_cancel_thread (GdnLisp *lisp);
gboolean              gdn_lisp_switch_thread (GdnLisp *lisp, int thd_idx);
void                  gdn_lisp_exit (GdnLisp *lisp);
void                  gdn_lisp_break (GdnLisp *lisp);
void                  gdn_lisp_continue (GdnLisp *lisp, GdnLispCommand command);
int                   gdn_lisp_get_input_fd (void);
int                   gdn_lisp_get_input_error_fd (void);
int                   gdn_lisp_get_input_prompt_fd (void);
int                   gdn_lisp_get_output_fd (void);
int                   gdn_lisp_set_input_callback (GdnLisp *            lisp,
                                                   GMainContext *       ctx,
                                                   GPollableSourceFunc *func,
                                                   gpointer             data);
GListStore *          gdn_lisp_get_environment (GdnLisp *lisp);
GListStore *          gdn_lisp_get_threads (GdnLisp *lisp);
GListStore *          gdn_lisp_get_modules (GdnLisp *lisp);
GListStore *          gdn_lisp_get_backtrace (GdnLisp *lisp);
char **               gdn_lisp_get_paths (void);
SCM                   gdn_lisp_get_default_thread (void);

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
