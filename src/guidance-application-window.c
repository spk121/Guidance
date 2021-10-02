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
#include <glib-unix.h>

struct _GdnApplicationWindow
{
  GtkApplicationWindow parent_instance;

  GtkStack *main_stack;

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

static void     activate_launch (GSimpleAction *simple,
                                 GVariant *     parameter,
                                 gpointer       user_data);
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

  self->thread_view = g_object_new (GDN_TYPE_THREAD_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->thread_window),
                                 GTK_WIDGET (self->thread_view));

  self->terminal_view = g_object_new (GDN_TYPE_TERMINAL_VIEW, NULL);
  gtk_box_append (self->terminal_box, self->terminal_view);
  gdn_terminal_view_connect_ports (self->terminal_view,
                                   gdn_lisp_get_input_fd (self->lisp),
                                   gdn_lisp_get_input_prompt_fd (self->lisp),
                                   gdn_lisp_get_input_error_fd (self->lisp),
                                   gdn_lisp_get_output_fd (self->lisp));

  self->source_view = g_object_new (GDN_TYPE_SOURCE_VIEW, NULL);
  gtk_box_append (self->source_box, self->source_view);
  scm_c_define ("*gdn-source-view*",
                gdn_source_view_to_scm (self->source_view));

  self->trap_view = g_object_new (GDN_TYPE_TRAP_VIEW, NULL);
  gtk_scrolled_window_set_child (self->trap_window, self->trap_view);
  scm_c_define ("*gdn-trap-view*", gdn_trap_view_to_scm (self->trap_view));

  self->module_view = g_object_new (GDN_TYPE_MODULE_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->module_window),
                                 GTK_WIDGET (self->module_view));

  self->environment_view = g_object_new (GDN_TYPE_ENVIRONMENT_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->environment_window),
                                 GTK_WIDGET (self->environment_view));
  scm_c_define ("*gdn-environment-view*",
                gdn_environment_view_to_scm (self->environment_view));

  self->backtrace_view = g_object_new (GDN_TYPE_BACKTRACE_VIEW, NULL);
  gtk_box_append (self->backtrace_box, self->backtrace_view);
  scm_c_define ("*gdn-backtrace-view*",
                gdn_backtrace_view_to_scm (self->backtrace_view));
  g_signal_connect_data (self->backtrace_view, "location",
                         G_CALLBACK (handle_backtrace_view_location), self,
                         NULL, 0);

  g_signal_connect (self->lisp, "after-gc", G_CALLBACK (handle_after_gc), self);
  g_signal_connect (self->lisp, "after-sweep", G_CALLBACK (handle_after_sweep),
                    self);
  g_signal_connect (self->module_view, "trap",
                    G_CALLBACK (handle_module_view_trap), self);
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
handle_backtrace_view_location (GdnBacktraceView *btview,
                                const char *      filename,
                                int               line,
                                int               col,
                                gpointer          user_data)
{
  g_assert_cmpstr (G_OBJECT_TYPE_NAME (user_data), ==, "GdnApplicationWindow");

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
