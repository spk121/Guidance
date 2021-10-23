/* guidance-application-window.c
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

#include "guidance-application-window.h"
#include "guidance-application.h"
#include "guidance-backtrace-view.h"
#include "guidance-binding-info.h"
#include "guidance-config.h"
#include "guidance-environment-view.h"
#include "guidance-frame-info.h"
#include "guidance-lisp.h"
#include "guidance-module-info.h"
#include "guidance-module-view.h"
#include "guidance-source-view.h"
#include "guidance-terminal-view.h"
#include "guidance-thread-view.h"
#include "guidance-trap-view.h"
#include "guidance-xgtk.h"
#include <glib-unix.h>

struct _GdnApplicationWindow
{
  GtkApplicationWindow parent_instance;

  GtkStack *main_stack;

  /* Toolbar */
  GtkButton *run_button;
  GtkButton *stop_button;
  GtkButton *step_over_button;
  GtkButton *step_over_instruction_button;
  GtkButton *step_into_button;
  GtkButton *step_into_instruction_button;
  GtkButton *step_out_button;
  GtkButton *menu_button;

  /* Terminal tab */
  GtkBox *terminal_box;
  GdnTerminalView *terminal_view;

  /* Threads tab */
  GtkScrolledWindow *thread_window;
  GdnThreadView *    thread_view;

  /* Traps tab */
  GtkScrolledWindow *trap_window;
  GdnTrapView *      trap_view;

  /* Module tab */
  GtkScrolledWindow *module_window;
  GdnModuleView *    module_view;

  /* Environment tab */
  GtkScrolledWindow * environment_window;
  GdnEnvironmentView *environment_view;

  /* Backtrace tab */
  GtkBox *backtrace_box;
  GdnBacktraceView *backtrace_view;

  /* Source tab */
  GtkBox *source_box;
  GdnSourceView *source_view;

  GtkImage *sweep_image;
  GtkImage *gc_image;

  /* Guile */
  GdnLisp *lisp;
};

G_DEFINE_TYPE (GdnApplicationWindow,
               gdn_application_window,
               GTK_TYPE_APPLICATION_WINDOW)


////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
static SCM scm_application_window_type = SCM_BOOL_F;
static SCM scm_repl_sym = SCM_BOOL_F;
static SCM scm_run_sym = SCM_BOOL_F;
static SCM scm_error_sym = SCM_BOOL_F;
static SCM scm_trap_sym = SCM_BOOL_F;

static SCM gdn_application_window_to_scm (GdnApplicationWindow *self);
static SCM scm_set_input_mode_x (SCM s_appwin, SCM s_mode);

static void     handle_css_parsing_error (GtkCssProvider *provider,
                                          GtkCssSection * section,
                                          GError *        error,
                                          gpointer        user_data);
static void     handle_after_gc (GdnLisp *lisp, gpointer user_data);
static void     handle_after_sweep (GdnLisp *lisp, gpointer user_data);
static void     handle_module_view_trap (GdnModuleView *view,
                                         guint64        packed_var,
                                         gpointer       user_data);
static void     show_page (GdnApplicationWindow *self, const char *name);

static void add_simple_action (GdnApplicationWindow *self,
                               const char *          name,
                               GCallback             callback);

static void handle_backtrace_view_location (GdnBacktraceView *view,
                                            const char *      filename,
                                            int               line,
                                            int               col,
                                            gpointer          user_data);
static void handle_module_view_location (GdnModuleView *view,
                                         const char *   filename,
                                         int            line,
                                         int            col,
                                         gpointer       user_data);

static void handle_entry (GdnTerminalView *view, char *str, gpointer user_data);
static void handle_step_into (GSimpleAction *simple,
                              GVariant *     parameter,
                              gpointer       user_data);
static void handle_step_into_instruction (GSimpleAction *simple,
                                          GVariant *     parameter,
                                          gpointer       user_data);
static void handle_step_over (GSimpleAction *simple,
                              GVariant *     parameter,
                              gpointer       user_data);
static void handle_step_over_instruction (GSimpleAction *simple,
                                          GVariant *     parameter,
                                          gpointer       user_data);
static void handle_step_out (GSimpleAction *simple,
                             GVariant *     parameter,
                             gpointer       user_data);
static void handle_process_run (GSimpleAction *simple,
                                GVariant *     parameter,
                                gpointer       user_data);
static void handle_process_continue (GSimpleAction *simple,
                                     GVariant *     parameter,
                                     gpointer       user_data);
static void handle_process_stop (GSimpleAction *simple,
                                 GVariant *     parameter,
                                 gpointer       user_data);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_application_window_class_init (GdnApplicationWindowClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/guidance-window.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnApplicationWindow, x)

  BIND (run_button);
  BIND (stop_button);
  BIND (step_over_button);
  BIND (step_over_instruction_button);
  BIND (step_into_button);
  BIND (step_into_instruction_button);
  BIND (step_out_button);
  BIND (menu_button);

  BIND (main_stack);
  BIND (terminal_box);
  BIND (thread_window);
  BIND (trap_window);
  BIND (module_window);
  BIND (environment_window);
  BIND (backtrace_box);
  BIND (source_box);
  BIND (sweep_image);
  BIND (gc_image);
#undef BIND
}

static void
gdn_widgets_guile_init (void *user_data)
{
  GdnApplicationWindow *self = user_data;
  scm_c_define ("*gdn-application-window*",
                gdn_application_window_to_scm (self));
  scm_c_define ("*gdn-backtrace-view*",
                gdn_backtrace_view_to_scm (self->backtrace_view));
  scm_c_define ("*gdn-environment-view*",
                gdn_environment_view_to_scm (self->environment_view));
  scm_c_define ("*gdn-source-view*",
                gdn_source_view_to_scm (self->source_view));
  scm_c_define ("*gdn-thread-view*",
                gdn_thread_view_to_scm (self->thread_view));
  scm_c_define ("*gdn-trap-view*", gdn_trap_view_to_scm (self->trap_view));
  scm_c_define ("*gdn-lisp*", gdn_lisp_to_scm (self->lisp));
  scm_c_export ("*gdn-application-window*", "*gdn-backtrace-view*",
                "*gdn-environment-view*", "*gdn-source-view*",
                "*gdn-thread-view*", "*gdn-trap-view*", "*gdn-lisp*", NULL);
}

static void
gdn_application_window_init (GdnApplicationWindow *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  /* Style and fashion */
  GtkCssProvider *provider = gtk_css_provider_new ();
  g_signal_connect (provider, "parsing-error",
                    G_CALLBACK (handle_css_parsing_error), NULL);
  gtk_css_provider_load_from_resource (
      provider, "/com/lonelycactus/Guidance/gtk/guidance-window.css");
  gtk_style_context_add_provider_for_display (
      gdk_display_get_default (), GTK_STYLE_PROVIDER (provider),
      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

  self->lisp = g_object_new (GDN_LISP_TYPE, NULL);

  /* All the pages */
  self->thread_view = g_object_new (GDN_TYPE_THREAD_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->thread_window),
                                 GTK_WIDGET (self->thread_view));

  self->terminal_view = g_object_new (GDN_TYPE_TERMINAL_VIEW, NULL);
  gtk_box_append (self->terminal_box, GTK_WIDGET (self->terminal_view));
  gdn_terminal_view_connect_ports (self->terminal_view,
                                   gdn_lisp_get_input_fd (self->lisp),
                                   gdn_lisp_get_input_prompt_fd (self->lisp),
                                   gdn_lisp_get_input_error_fd (self->lisp),
                                   gdn_lisp_get_output_fd (self->lisp));
  g_signal_connect (self->terminal_view, "entry", G_CALLBACK (handle_entry),
                    self);

  self->source_view = g_object_new (GDN_TYPE_SOURCE_VIEW, NULL);
  gtk_box_append (self->source_box, GTK_WIDGET (self->source_view));

  self->trap_view = g_object_new (GDN_TYPE_TRAP_VIEW, NULL);
  gtk_scrolled_window_set_child (self->trap_window,
                                 GTK_WIDGET (self->trap_view));

  self->module_view = g_object_new (GDN_TYPE_MODULE_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->module_window),
                                 GTK_WIDGET (self->module_view));
  g_signal_connect_data (self->module_view, "location",
                         G_CALLBACK (handle_module_view_location), self, NULL,
                         0);

  self->environment_view = g_object_new (GDN_TYPE_ENVIRONMENT_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->environment_window),
                                 GTK_WIDGET (self->environment_view));

  self->backtrace_view = g_object_new (GDN_TYPE_BACKTRACE_VIEW, NULL);
  gtk_box_append (self->backtrace_box, GTK_WIDGET (self->backtrace_view));
  g_signal_connect_data (self->backtrace_view, "location",
                         G_CALLBACK (handle_backtrace_view_location), self,
                         NULL, 0);

  g_signal_connect (self->lisp, "after-gc", G_CALLBACK (handle_after_gc), self);
  g_signal_connect (self->lisp, "after-sweep", G_CALLBACK (handle_after_sweep),
                    self);
  g_signal_connect (self->module_view, "trap",
                    G_CALLBACK (handle_module_view_trap), self);

  /* Simple actions */
  add_simple_action (self, "step-into", G_CALLBACK (handle_step_into));
  add_simple_action (self, "step-into-instruction",
                     G_CALLBACK (handle_step_into_instruction));
  add_simple_action (self, "step-over", G_CALLBACK (handle_step_over));
  add_simple_action (self, "step-over-instruction",
                     G_CALLBACK (handle_step_over_instruction));
  add_simple_action (self, "step-out", G_CALLBACK (handle_step_out));
  add_simple_action (self, "process-run", G_CALLBACK (handle_process_run));
  add_simple_action (self, "process-continue",
                     G_CALLBACK (handle_process_continue));
  add_simple_action (self, "process-stop", G_CALLBACK (handle_process_stop));

  scm_c_define_module ("gdn widgets", gdn_widgets_guile_init, self);

  gdn_lisp_run (self->lisp);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////


static void
handle_css_parsing_error (GtkCssProvider *provider,
                          GtkCssSection * section,
                          GError *        error,
                          gpointer        user_data)
{
  g_assert (provider != NULL);
  g_assert (section != NULL);
  g_assert (user_data == NULL);

  g_error ("CSS parsing error: %s", error->message);
}

static gboolean
clear_gc (gpointer user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

  GdnApplicationWindow *self = user_data;

  gtk_widget_set_visible (GTK_WIDGET (self->gc_image), FALSE);
  return G_SOURCE_REMOVE;
}

static void
handle_after_gc (GdnLisp *lisp, gpointer user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (lisp), ==, "GdnLisp");
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

  GdnApplicationWindow *self = user_data;

  /* When called, we reveal the sweep image for a second. */
  gtk_widget_set_visible (GTK_WIDGET (self->gc_image), TRUE);
  g_timeout_add (2000, clear_gc, self);
}

static gboolean
clear_sweep (gpointer user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

  GdnApplicationWindow *self = user_data;
  gtk_widget_set_visible (GTK_WIDGET (self->sweep_image), FALSE);
  return G_SOURCE_REMOVE;
}

static void
handle_after_sweep (GdnLisp *lisp, gpointer user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (lisp), ==, "GdnLisp");
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

  GdnApplicationWindow *self = user_data;

  /* When called, we reveal the sweep image for a second. */
  gtk_widget_set_visible (GTK_WIDGET (self->sweep_image), TRUE);
  g_timeout_add (2000, clear_sweep, self);
}

static void
handle_backtrace_view_location (G_GNUC_UNUSED GdnBacktraceView *btview,
                                const char *                    filename,
                                int                             line,
                                int                             col,
                                gpointer                        user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

  GdnApplicationWindow *self = user_data;
  gdn_source_view_show_location (self->source_view, filename, line, col);
  show_page (self, "source");
}

static void
handle_module_view_location (G_GNUC_UNUSED GdnModuleView *mview,
                             const char *                 filename,
                             int                          line,
                             int                          col,
                             gpointer                     user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_source_view_show_location (self->source_view, filename, line, col);
  show_page (self, "source");
}

static void
handle_module_view_trap (GdnModuleView *view,
                         guint64        packed_var,
                         gpointer       user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (view), ==, "GdnModuleView");
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

  GdnApplicationWindow *self = user_data;
  scm_t_bits            x_bits = (scm_t_bits) packed_var;
  SCM                   proc = SCM_PACK (x_bits);
  gdn_lisp_add_proc_trap_async (self->lisp, proc);
}

static void
handle_entry (G_GNUC_UNUSED GdnTerminalView *view,
              char *                         str,
              gpointer                       user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_EVAL, str);
}

static void
handle_step_into (G_GNUC_UNUSED GSimpleAction *simple,
                  G_GNUC_UNUSED GVariant *parameter,
                  gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_STEP_INTO, NULL);
}

static void
handle_step_into_instruction (G_GNUC_UNUSED GSimpleAction *simple,
                              G_GNUC_UNUSED GVariant *parameter,
                              gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp,
                              GDN_LISP_COMMAND_STEP_INTO_INSTRUCTION, NULL);
}

static void
handle_step_over (G_GNUC_UNUSED GSimpleAction *simple,
                  G_GNUC_UNUSED GVariant *parameter,
                  gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_STEP, NULL);
}

static void
handle_step_over_instruction (G_GNUC_UNUSED GSimpleAction *simple,
                              G_GNUC_UNUSED GVariant *parameter,
                              gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_STEP_INSTRUCTION,
                              NULL);
}

static void
handle_step_out (G_GNUC_UNUSED GSimpleAction *simple,
                 G_GNUC_UNUSED GVariant *parameter,
                 gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_STEP_OUT, NULL);
}

static void
handle_process_run (G_GNUC_UNUSED GSimpleAction *simple,
                    G_GNUC_UNUSED GVariant *parameter,
                    gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_RUN, NULL);
}

static void
handle_process_continue (G_GNUC_UNUSED GSimpleAction *simple,
                         G_GNUC_UNUSED GVariant *parameter,
                         gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_CONTINUE, NULL);
}

static void
handle_process_stop (G_GNUC_UNUSED GSimpleAction *simple,
                     G_GNUC_UNUSED GVariant *parameter,
                     gpointer                user_data)
{
  GdnApplicationWindow *self = user_data;
  gdn_lisp_set_user_response (self->lisp, GDN_LISP_COMMAND_STOP, NULL);
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static void
add_simple_action (GdnApplicationWindow *self,
                   const char *          name,
                   GCallback             callback)
{
  GSimpleAction *action;
  action = g_simple_action_new (name, NULL);
  if (callback != NULL)
    g_signal_connect (action, "activate", callback, self);
  g_action_map_add_action (G_ACTION_MAP (self), G_ACTION (action));
}

static void
show_page (GdnApplicationWindow *self, const char *name)
{
  GtkWidget *wigz = gtk_stack_get_child_by_name (self->main_stack, name);
  if (wigz != NULL)
    gtk_stack_set_visible_child (self->main_stack, wigz);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

static SCM
gdn_application_window_to_scm (GdnApplicationWindow *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_APPLICATION_WINDOW);
  return scm_make_foreign_object_1 (scm_application_window_type, self);
}

static SCM
scm_set_input_mode_x (SCM s_appwin, SCM s_mode)
{
  scm_assert_foreign_object_type (scm_application_window_type, s_appwin);
  GdnApplicationWindow *self = scm_foreign_object_ref (s_appwin, 0);

  if (scm_is_eq (s_mode, scm_repl_sym))
    {
      xgtk_button_set_sensitive (self->run_button, TRUE);
      // xgtk_button_set_sensitive (self->continue_button, FALSE);
      xgtk_button_set_sensitive (self->stop_button, FALSE);
      xgtk_button_set_sensitive (self->step_into_button, FALSE);
      xgtk_button_set_sensitive (self->step_into_instruction_button, FALSE);
      xgtk_button_set_sensitive (self->step_over_button, FALSE);
      xgtk_button_set_sensitive (self->step_over_instruction_button, FALSE);
      xgtk_button_set_sensitive (self->step_out_button, FALSE);
      gdn_terminal_view_set_port_mode (self->terminal_view, FALSE);
    }
  else if (scm_is_eq (s_mode, scm_run_sym))
    {
      xgtk_button_set_sensitive (self->run_button, FALSE);
      // xgtk_button_set_sensitive (self->continue_button, FALSE);
      xgtk_button_set_sensitive (self->stop_button, TRUE);
      xgtk_button_set_sensitive (self->step_into_button, FALSE);
      xgtk_button_set_sensitive (self->step_into_instruction_button, FALSE);
      xgtk_button_set_sensitive (self->step_over_button, FALSE);
      xgtk_button_set_sensitive (self->step_over_instruction_button, FALSE);
      xgtk_button_set_sensitive (self->step_out_button, FALSE);
      gdn_terminal_view_set_port_mode (self->terminal_view, TRUE);
    }
  else if (scm_is_eq (s_mode, scm_error_sym))
    {
      xgtk_button_set_sensitive (self->run_button, FALSE);
      // xgtk_button_set_sensitive (self->continue_button, FALSE);
      xgtk_button_set_sensitive (self->stop_button, TRUE);
      xgtk_button_set_sensitive (self->step_into_button, FALSE);
      xgtk_button_set_sensitive (self->step_into_instruction_button, FALSE);
      xgtk_button_set_sensitive (self->step_over_button, FALSE);
      xgtk_button_set_sensitive (self->step_over_instruction_button, FALSE);
      xgtk_button_set_sensitive (self->step_out_button, FALSE);
      gdn_terminal_view_set_port_mode (self->terminal_view, FALSE);
    }
  else if (scm_is_eq (s_mode, scm_trap_sym))
    {
      xgtk_button_set_sensitive (self->run_button, TRUE);
      // xgtk_button_set_sensitive (self->continue_button, TRUE);
      xgtk_button_set_sensitive (self->stop_button, TRUE);
      xgtk_button_set_sensitive (self->step_into_button, TRUE);
      xgtk_button_set_sensitive (self->step_into_instruction_button, TRUE);
      xgtk_button_set_sensitive (self->step_over_button, TRUE);
      xgtk_button_set_sensitive (self->step_over_instruction_button, TRUE);
      xgtk_button_set_sensitive (self->step_out_button, TRUE);
      gdn_terminal_view_set_port_mode (self->terminal_view, FALSE);
    }
  else
    {
      g_critical ("unknown mode in set-input-mode-x!");
    }
  return SCM_UNSPECIFIED;
}

void
gdn_application_window_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-application-window");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_application_window_type =
      scm_make_foreign_object_type (name, slots, NULL);

  scm_repl_sym = scm_from_utf8_symbol ("repl");
  scm_run_sym = scm_from_utf8_symbol ("run");
  scm_error_sym = scm_from_utf8_symbol ("error");
  scm_trap_sym = scm_from_utf8_symbol ("trap");

  scm_c_define_gsubr ("gdn-set-input-mode!", 2, 0, 0, scm_set_input_mode_x);
  scm_c_export ("gdn-set-input-mode!", NULL);
}
