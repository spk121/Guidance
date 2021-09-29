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
#include "guidance-environment-info.h"
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

  /* Traps tab */
  GtkScrolledWindow *trap_window;

  /* Module tab */
  GtkScrolledWindow *module_window;
  GdnModuleView *    module_view;

  /* Environment tab */
  GtkColumnView *      environment_column_view;
  GtkColumnViewColumn *environment_key_column;
  GtkColumnViewColumn *environment_value_column;

  /* Backtrace tab */
  GtkBox *backtrace_box;

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

static void add_simple_action (GdnApplicationWindow *self,
                               const char *          name,
                               GCallback             callback);

static void environment_key_setup (GtkListItemFactory *factory,
                                   GtkListItem *       list_item);
static void environment_key_bind (GtkListItemFactory *factory,
                                  GtkListItem *       list_item);
static void environment_key_unbind (GtkListItemFactory *factory,
                                    GtkListItem *       list_item);
static void environment_value_setup (GtkListItemFactory *factory,
                                     GtkListItem *       list_item);
static void environment_value_bind (GtkListItemFactory *factory,
                                    GtkListItem *       list_item);
static void environment_value_unbind (GtkListItemFactory *factory,
                                      GtkListItem *       list_item);
static void handle_backtrace_view_location (GdnBacktraceView *view,
                                            const char *      filename,
                                            int               line,
                                            int               col,
                                            gpointer          user_data);
// static void environment_activate(GtkListView *list, guint position, gpointer
// unused);
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

  /* Terminal tab */
  BIND (terminal_box);

  /* Threads tab */
  BIND (thread_window);

  /* Trap tab */
  BIND (trap_window);

  /* Modules tab */
  BIND (module_window);

  /* Environment tab */
  BIND (environment_column_view);
  BIND (environment_key_column);
  BIND (environment_value_column);

  /* Backtrace tab */
  BIND (backtrace_box);

  /* Source Tab */
  BIND (source_box);

  BIND (sweep_image);
  BIND (gc_image);
#undef BIND
}

static void
application_window_init_threads_tab (GdnApplicationWindow *self)
{
  GdnThreadView *thread_view;

  thread_view = g_object_new (GDN_TYPE_THREAD_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->thread_window),
                                 GTK_WIDGET (thread_view));
}

static void
application_window_init_environment_tab (GdnApplicationWindow *self)
{
  GtkTreeListModel *tree_model;
  tree_model = gdn_environment_info_get_tree_model ();

  /* Here we create the a list viewer with columns. It needs to know
   * up front how we're handling selections. We want to be able to
   * handle a single line at a time. The selection model tries to own
   * the tree model, but we ref it since we're freeing it a different
   * way. */
  GtkSingleSelection *selection_model;
  selection_model = gtk_single_selection_new (
      G_LIST_MODEL (g_object_ref (G_OBJECT (tree_model))));
  // g_signal_connect(selection_model, "selection-changed",
  // environment_selection_changed, self);
  gtk_column_view_set_model (self->environment_column_view,
                             GTK_SELECTION_MODEL (selection_model));

  /* This column view has a key and a value column. We need to set up
   * factories to create the cells in each column. */
  GtkSignalListItemFactory *environment_key_column_factory;
  environment_key_column_factory =
      GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (environment_key_column_factory, "setup",
                    G_CALLBACK (environment_key_setup), self);
  g_signal_connect (environment_key_column_factory, "bind",
                    G_CALLBACK (environment_key_bind), self);
  g_signal_connect (environment_key_column_factory, "unbind",
                    G_CALLBACK (environment_key_unbind), self);
  gtk_column_view_column_set_factory (
      self->environment_key_column,
      GTK_LIST_ITEM_FACTORY (environment_key_column_factory));

  GtkSignalListItemFactory *environment_value_column_factory;
  environment_value_column_factory =
      GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (environment_value_column_factory, "setup",
                    G_CALLBACK (environment_value_setup), self);
  g_signal_connect (environment_value_column_factory, "bind",
                    G_CALLBACK (environment_value_bind), self);
  g_signal_connect (environment_value_column_factory, "unbind",
                    G_CALLBACK (environment_value_unbind), self);
  gtk_column_view_column_set_factory (
      self->environment_value_column,
      GTK_LIST_ITEM_FACTORY (environment_value_column_factory));
}

static void
application_window_init_backtrace_tab (GdnApplicationWindow *self)
{
  GdnBacktraceView *backtrace_view;

  backtrace_view = g_object_new (GDN_TYPE_BACKTRACE_VIEW, NULL);
  gtk_box_append (self->backtrace_box, backtrace_view);
  scm_c_define ("*gdn-backtrace-view*",
                gdn_backtrace_view_to_scm (backtrace_view));
  g_signal_connect_data (backtrace_view, "location",
                         G_CALLBACK (handle_backtrace_view_location), self,
                         NULL, 0);
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

  application_window_init_threads_tab (self);
  self->terminal_view = g_object_new (GDN_TYPE_TERMINAL_VIEW, NULL);
  gtk_box_append (self->terminal_box, self->terminal_view);

  self->source_view = g_object_new (GDN_TYPE_SOURCE_VIEW, NULL);
  gtk_box_append (self->source_box, self->source_view);
  scm_c_define ("*gdn-source-view*",
                gdn_source_view_to_scm (self->source_view));

  self->module_view = g_object_new (GDN_TYPE_MODULE_VIEW, NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->module_window),
                                 GTK_WIDGET (self->module_view));

  application_window_init_environment_tab (self);
  application_window_init_backtrace_tab (self);

  gdn_terminal_view_connect_lisp_ports (self->terminal_view, self->lisp);

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
  gdn_application_window_show_page (self, "source");
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
environment_key_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_assert (factory != NULL);

  GtkExpander *expander;
  GtkLabel *   label;

  expander = gtk_tree_expander_new ();
  gtk_list_item_set_child (list_item, expander);

  label = gtk_label_new (NULL);
  gtk_widget_set_margin_start (label, 5);
  gtk_widget_set_margin_end (label, 5);
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), label);
}

static void
environment_key_bind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_assert (factory != NULL);

  GtkTreeListRow *list_row;
  GtkWidget *     expander;
  GtkWidget *     label;
  gpointer        item;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);

  expander = gtk_list_item_get_child (list_item);
  gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (expander), list_row);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));

  gtk_label_set_label (GTK_LABEL (label), gdn_environment_info_get_key (item));
}

static void
environment_key_unbind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_assert (factory != NULL);

  GtkWidget *     expander;
  GtkWidget *     label;

  expander = gtk_list_item_get_child (list_item);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));
  gtk_label_set_label (GTK_LABEL (label), "");
}

static void
environment_value_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_assert (factory != NULL);

  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_widget_set_margin_start (label, 5);
  gtk_widget_set_margin_end (label, 5);
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_list_item_set_child (list_item, label);
}

static void
environment_value_bind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_assert (factory != NULL);

  GtkTreeListRow *list_row;
  GtkLabel *      label;
  gpointer        item;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_label (label, gdn_environment_info_get_value (item));
}

static void
environment_value_unbind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_assert (factory != NULL);

  GtkWidget *     label;

  label = gtk_list_item_get_child (list_item);
  gtk_label_set_label (GTK_LABEL (label), "");
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

void
gdn_application_window_show_page (GdnApplicationWindow *self, const char *name)
{
  GtkWidget *wigz = gtk_stack_get_child_by_name (self->main_stack, name);
  if (wigz != NULL)
    gtk_stack_set_visible_child (self->main_stack, wigz);
}
