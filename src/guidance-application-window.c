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
#include "guidance-config.h"
#include "guidance-environment-info.h"
#include "guidance-frame-info.h"
#include "guidance-lisp.h"
#include "guidance-module-info.h"
#include "guidance-thread-info.h"
#include <glib-unix.h>

struct _GdnApplicationWindow
{
  GtkApplicationWindow parent_instance;

  GtkStack *main_stack;

  /* Settings tab */
  GtkCheckButton *settings_start_repl_radio;
  GtkCheckButton *settings_use_args_radio;
  GtkEntry *      settings_args_entry;
  GtkSwitch *     settings_pause_switch;

  /* Interpreter and terminal tab */
  GdnLisp *      lisp;
  GList *        history;
  GList *        history_cur;
  GtkTextView *  terminal_text_view;
  GtkTextBuffer *terminal_text_buffer;
  GtkLabel *     terminal_prompt_label;
  GtkEntry *     terminal_input_entry;

  /* Threads tab */
  GtkScrolledWindow *thread_window;
  GtkColumnView *      thread_column_view;
  GtkColumnViewColumn *thread_name_column;
  GtkColumnViewColumn *thread_emoji_column;
  GtkColumnViewColumn *thread_active_column;

  /* Threads tab */
  GtkScrolledWindow *module_window;

  /* Environment tab */
  GtkColumnView *      environment_column_view;
  GtkColumnViewColumn *environment_key_column;
  GtkColumnViewColumn *environment_value_column;

  /* Backtrace tab */
  GtkListView *        backtrace_stack_column_view;
  GtkColumnView *      backtrace_variables_column_view;
  GtkColumnViewColumn *backtrace_variables_name_column;
  GtkColumnViewColumn *backtrace_variables_representation_column;
  GtkColumnViewColumn *backtrace_variables_value_column;
  GtkColumnViewColumn *backtrace_variables_info_column;
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
static void     activate_terminal_entry (GtkEntry *entry, gpointer user_data);
static gboolean
poll_terminal_text2 (gint fd, GIOCondition condition, gpointer user_data);
static gboolean
poll_terminal_prompt2 (gint fd, GIOCondition condition, gpointer user_data);
static gboolean
poll_terminal_error2 (gint fd, GIOCondition condition, gpointer user_data);

static void add_simple_action (GdnApplicationWindow *self,
                               const char *          name,
                               GCallback             callback);
static char *xread (int fd);
static void  thread_setup (GtkListItemFactory *factory, GtkListItem *list_item);
static void  thread_teardown (GtkListItemFactory *factory,
                              GtkListItem *       list_item);
static void  thread_bind (GtkListItemFactory *factory, GtkListItem *list_item);
static void
thread_activate (GtkListView *list, guint position, gpointer unused);

static void module_setup (GtkListItemFactory *factory, GtkListItem *list_item);
static void module_teardown (GtkListItemFactory *factory,
                             GtkListItem *       list_item);
static void module_bind (GtkListItemFactory *factory, GtkListItem *list_item);
static void
module_activate (GtkListView *list, guint position, gpointer unused);

static void environment_key_setup (GtkListItemFactory *factory,
                                   GtkListItem *       list_item);
static void environment_key_bind (GtkListItemFactory *factory,
                                  GtkListItem *       list_item);
static void environment_key_unbind (GtkListItemFactory *factory,
                                    GtkListItem *       list_item);
static void environment_key_teardown (GtkListItemFactory *factory,
                                      GtkListItem *       list_item);
static void environment_value_setup (GtkListItemFactory *factory,
                                     GtkListItem *       list_item);
static void environment_value_bind (GtkListItemFactory *factory,
                                    GtkListItem *       list_item);
static void environment_value_unbind (GtkListItemFactory *factory,
                                      GtkListItem *       list_item);
static void environment_value_teardown (GtkListItemFactory *factory,
                                        GtkListItem *       list_item);

// static void environment_activate(GtkListView *list, guint position, gpointer
// unused);

static void backtrace_setup (GtkListItemFactory *factory,
                             GtkListItem *       list_item);
static void backtrace_teardown (GtkListItemFactory *factory,
                                GtkListItem *       list_item);
static void backtrace_bind (GtkListItemFactory *factory,
                            GtkListItem *       list_item);
static void
backtrace_activate (GtkListView *list, guint position, gpointer unused);

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

  /* Settings Tab */
  BIND (settings_start_repl_radio);
  BIND (settings_use_args_radio);
  BIND (settings_args_entry);
  BIND (settings_pause_switch);

  /* Terminal tab */
  BIND (terminal_text_view);
  BIND (terminal_text_buffer);
  BIND (terminal_prompt_label);
  BIND (terminal_input_entry);

  /* Threads tab */
  BIND (thread_window);

  /* Modules tab */
  BIND (module_window);

  /* Environment tab */
  BIND (environment_column_view);
  BIND (environment_key_column);
  BIND (environment_value_column);

  /* Backtrace tab */
  BIND (backtrace_stack_column_view);
  BIND (backtrace_variables_column_view);
  BIND (backtrace_variables_name_column);
  BIND (backtrace_variables_representation_column);
  BIND (backtrace_variables_value_column);
  BIND (backtrace_variables_info_column);
}

static void
application_window_init_settings_tab (GdnApplicationWindow *self)
{
  GdnApplication *app;
  GtkEntryBuffer *buf;

  app = GDN_APPLICATION (g_application_get_default ());
  buf = gtk_entry_get_buffer (self->settings_args_entry);

  gtk_check_button_set_group (self->settings_use_args_radio,
                              self->settings_start_repl_radio);
  if (gdn_application_get_argc (app) > 1)
    {
      /* Copy the command-line arguments into this buffer. */
      gtk_entry_buffer_set_text (buf, gdn_application_get_args (app), -1);
      gtk_check_button_set_active (self->settings_use_args_radio, TRUE);
    }
  else
    {
      gtk_check_button_set_active (self->settings_start_repl_radio, TRUE);
    }
}

static void
application_window_init_lisp_and_terminal_tab (GdnApplicationWindow *self)
{
  self->lisp = gdn_lisp_new ();
  self->history = NULL;
  self->history_cur = NULL;

  g_unix_fd_add_full (G_PRIORITY_DEFAULT, gdn_lisp_get_input_fd (self->lisp),
                      G_IO_IN, poll_terminal_text2, self, NULL);
  g_unix_fd_add_full (G_PRIORITY_DEFAULT,
                      gdn_lisp_get_input_prompt_fd (self->lisp), G_IO_IN,
                      poll_terminal_prompt2, self, NULL);
  g_unix_fd_add_full (G_PRIORITY_DEFAULT,
                      gdn_lisp_get_input_error_fd (self->lisp), G_IO_IN,
                      poll_terminal_error2, self, NULL);
  g_signal_connect (self->terminal_input_entry, "activate",
                    G_CALLBACK (activate_terminal_entry), self);
}

static void
application_window_init_threads_tab (GdnApplicationWindow *self)
{
  GListModel *              model;
  GtkSignalListItemFactory *factory;
  GtkListView *             listview;

  model = G_LIST_MODEL (gdn_lisp_get_threads (self->lisp));
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (factory, "setup", G_CALLBACK (thread_setup), NULL);
  g_signal_connect (factory, "teardown", G_CALLBACK (thread_teardown), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (thread_bind), NULL);
  listview = GTK_LIST_VIEW (
      gtk_list_view_new (GTK_SELECTION_MODEL (gtk_single_selection_new (model)),
                         GTK_LIST_ITEM_FACTORY (factory)));
  g_signal_connect (listview, "activate", G_CALLBACK (thread_activate), NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->thread_window),
                                 GTK_WIDGET (listview));
}

static void
application_window_init_modules_tab (GdnApplicationWindow *self)
{
  GListModel *              model;
  GtkSignalListItemFactory *factory;
  GtkListView *             listview;

  model = G_LIST_MODEL (gdn_lisp_get_modules (self->lisp));
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());

  g_signal_connect (factory, "setup", G_CALLBACK (module_setup), NULL);
  g_signal_connect (factory, "teardown", G_CALLBACK (module_teardown), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (module_bind), NULL);

  listview = GTK_LIST_VIEW (
      gtk_list_view_new (GTK_SELECTION_MODEL (gtk_single_selection_new (model)),
                         GTK_LIST_ITEM_FACTORY (factory)));

  g_signal_connect (listview, "activate", G_CALLBACK (module_activate), NULL);
  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (self->module_window),
                                 GTK_WIDGET (listview));
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
  gtk_column_view_set_model (self->environment_column_view, selection_model);

  /* This column view has a key and a value column. We need to set up
   * factories to create the cells in each column. */
  GtkSignalListItemFactory *environment_key_column_factory;
  environment_key_column_factory =
      GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (environment_key_column_factory, "setup",
                    environment_key_setup, self);
  g_signal_connect (environment_key_column_factory, "bind",
                    environment_key_bind, self);
  g_signal_connect (environment_key_column_factory, "unbind",
                    environment_key_unbind, self);
  g_signal_connect (environment_key_column_factory, "teardown",
                    environment_key_teardown, self);
  gtk_column_view_column_set_factory (self->environment_key_column,
                                      environment_key_column_factory);

  GtkSignalListItemFactory *environment_value_column_factory;
  environment_value_column_factory = gtk_signal_list_item_factory_new ();
  g_signal_connect (environment_value_column_factory, "setup",
                    environment_value_setup, self);
  g_signal_connect (environment_value_column_factory, "bind",
                    environment_value_bind, self);
  g_signal_connect (environment_value_column_factory, "unbind",
                    environment_value_unbind, self);
  g_signal_connect (environment_value_column_factory, "teardown",
                    environment_value_teardown, self);
  gtk_column_view_column_set_factory (self->environment_value_column,
                                      environment_value_column_factory);
}

static void
application_window_init_backtrace_tab (GdnApplicationWindow *self)
{
  GListModel *              model;
  GtkSignalListItemFactory *factory;
  GtkListView *             listview;

  model = G_LIST_MODEL (gdn_lisp_get_backtrace (self->lisp));
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());

  g_signal_connect (factory, "setup", G_CALLBACK (backtrace_setup), NULL);
  g_signal_connect (factory, "teardown", G_CALLBACK (backtrace_teardown), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (backtrace_bind), NULL);

  listview = GTK_LIST_VIEW (
      gtk_list_view_new (GTK_SELECTION_MODEL (gtk_single_selection_new (model)),
                         GTK_LIST_ITEM_FACTORY (factory)));
  g_signal_connect (listview, "activate", G_CALLBACK (backtrace_activate),
                    NULL);
}

static void
gdn_application_window_init (GdnApplicationWindow *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  add_simple_action (self, "launch", G_CALLBACK (activate_launch));

  application_window_init_settings_tab (self);
  application_window_init_lisp_and_terminal_tab (self);
  application_window_init_threads_tab (self);
  application_window_init_modules_tab (self);
  application_window_init_environment_tab (self);
  application_window_init_backtrace_tab (self);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

/* The operator has hit the launch button. Kick off a new thread
 * in the Lisp interpreter */
static void
activate_launch (G_GNUC_UNUSED GSimpleAction *simple,
                 G_GNUC_UNUSED GVariant *parameter,
                 gpointer                user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  if (gtk_check_button_get_active (self->settings_start_repl_radio))
    {
      g_debug ("launch repl requested");
      gdn_lisp_spawn_repl_thread (self->lisp);
      GtkWidget *wigz =
          gtk_stack_get_child_by_name (self->main_stack, "terminal");
      if (wigz != NULL)
        gtk_stack_set_visible_child (self->main_stack, wigz);
    }
  else if (gtk_check_button_get_active (self->settings_use_args_radio))
    {
      gboolean pause = gtk_switch_get_active (self->settings_pause_switch);
      GtkEntryBuffer *buf = gtk_entry_get_buffer (self->settings_args_entry);
      const char *    args = gtk_entry_buffer_get_text (buf);
      gdn_lisp_spawn_args_thread (self->lisp, args, pause);
    }
}

/* The operater has hit enter on the GtkEntry in the Terminal widget.
 * Send that text to the Lisp interpreter. */
static void
activate_terminal_entry (GtkEntry *entry, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  const char *          text;

  text = gtk_entry_buffer_get_text (gtk_entry_get_buffer (entry));
  int fd = gdn_lisp_get_output_fd (self->lisp);
  int bytes_written = write (fd, text, strlen (text));
  if (bytes_written == -1)
    g_critical ("Could not push text to the Guile interpreter");
  if (write (fd, "\n", 1) == -1)
    g_critical ("Could not push newline to the Guile interpreter");
  fsync (fd);

  /* Write the prompt and the text from the input GtkEntry box onto the main
   * output widget */
  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  GtkTextIter    iter_start, iter_end;
  GtkTextMark *  mark_start;
  const char *   prompt = gtk_label_get_text (self->terminal_prompt_label);

  /* Store the 'before' location */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start,
                                    gtk_text_buffer_get_insert (text_buffer));
  mark_start =
      gtk_text_buffer_create_mark (text_buffer, NULL, &iter_start, TRUE);

  /* Write the prompt and the input text to the main output widget */
  gtk_text_buffer_insert_at_cursor (text_buffer, prompt, strlen (prompt));
  gtk_text_buffer_insert_at_cursor (text_buffer, text, strlen (text));
  gtk_text_buffer_insert_at_cursor (text_buffer, "\n", 1);

  /* Change the presentation for the prompt and entry text */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start, mark_start);
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_end,
                                    gtk_text_buffer_get_insert (text_buffer));
  gtk_text_buffer_apply_tag_by_name (text_buffer, "input", &iter_start,
                                     &iter_end);
  gtk_text_buffer_delete_mark (text_buffer, mark_start);

  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));

  /* Append the entry to the history */
  if (!self->history || strncmp (text, self->history->data, strlen (text)) != 0)
    self->history =
        g_list_prepend (self->history, strndup (text, strlen (text)));
  self->history_cur = self->history;

  /* Then clear the entry box. */
  gtk_entry_buffer_set_text (gtk_entry_get_buffer (self->terminal_input_entry),
                             "", 0);
}

gboolean
poll_terminal_text2 (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  // g_critical ("POLL TERMINAL");
  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);

  /* Get all the text from the read port. */
  char *buf = xread (fd);
  int   len = strlen (buf);

  /* Copy it over to the GtkTextView output widget. */
  if (len > 0)
    gtk_text_buffer_insert_at_cursor (text_buffer, buf, len);
  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));
  g_free (buf);

  return TRUE;
}

gboolean
poll_terminal_error2 (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);

  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  char *         buf = xread (fd);
  int            len = strlen (buf);

  GtkTextIter  iter_start, iter_end;
  GtkTextMark *mark_start;

  /* Store the 'before' location */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start,
                                    gtk_text_buffer_get_insert (text_buffer));
  mark_start =
      gtk_text_buffer_create_mark (text_buffer, NULL, &iter_start, TRUE);

  /* Copy it over to the GtkTextView output widget. */
  gtk_text_buffer_insert_at_cursor (text_buffer, buf, len);
  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));

  /* Change the presentation for the error text */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start, mark_start);
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_end,
                                    gtk_text_buffer_get_insert (text_buffer));
  gtk_text_buffer_apply_tag_by_name (text_buffer, "error", &iter_start,
                                     &iter_end);
  gtk_text_buffer_delete_mark (text_buffer, mark_start);
  g_free (buf);

  return TRUE;
}

gboolean
poll_terminal_prompt2 (gint fd, GIOCondition condition, gpointer user_data)
{
#define MAX_PROMPT_CODEPOINTS 80
#define MAX_PROMPT_BYTES (MAX_PROMPT_CODEPOINTS * 3)

  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  char *                buf;

  /* Clear out the old label */
  gtk_label_set_text (self->terminal_prompt_label, "");

  /* Read from the port */
  buf = xread (fd);

  /* How big a prompt should we handle? */
  glong u8_len;
  u8_len = g_utf8_strlen (buf, MAX_PROMPT_BYTES);
  if (u8_len > MAX_PROMPT_CODEPOINTS)
    {
      gchar *substr = g_utf8_substring (buf, 0, MAX_PROMPT_CODEPOINTS);
      gtk_label_set_text (self->terminal_prompt_label, substr);
      g_free (substr);
    }
  else if (u8_len > 0)
    gtk_label_set_text (self->terminal_prompt_label, buf);

  g_free (buf);

  return TRUE;
}

static void
thread_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
thread_teardown (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *label;

  label = gtk_list_item_get_child (list_item);
  if (label)
    g_object_unref (label);
}

static void
thread_bind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *     label;
  GdnThreadInfo *info;

  label = gtk_list_item_get_child (list_item);
  GObject *obj = gtk_list_item_get_item (list_item);
  if (list_item)
    {
      info = GDN_THREAD_INFO (obj);
      gtk_label_set_text (label, gdn_thread_info_get_name (info));
    }
  g_object_unref (obj);
}

static void
thread_activate (GtkListView *list, guint position, gpointer unused)
{
  // gboolean              gdn_lisp_switch_thread (GdnLisp *lisp, int thd_idx);
}

static void
module_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_list_item_set_child (list_item, label);
}

static void
module_teardown (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *label;

  label = gtk_list_item_get_child (list_item);
  if (label)
    g_object_unref (label);
}

static void
module_bind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *     label;
  GdnThreadInfo *info;

  label = gtk_list_item_get_child (list_item);
  GObject *obj = gtk_list_item_get_item (list_item);
  if (list_item)
    {
      info = GDN_MODULE_INFO (obj);
      gtk_label_set_text (label, gdn_thread_info_get_name (info));
    }
  g_object_unref (obj);
}

static void
module_activate (GtkListView *list, guint position, gpointer unused)
{
  // gboolean              gdn_lisp_switch_thread (GdnLisp *lisp, int thd_idx);
}

static void
environment_key_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
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
  GtkTreeListRow *list_row;
  GtkWidget *     expander;
  GtkWidget *     label;
  gpointer        item;

  list_row = gtk_list_item_get_item (list_item);
  expander = gtk_list_item_get_child (list_item);
  gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (expander), list_row);
  item = gtk_tree_list_row_get_item (list_row);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));

  gtk_label_set_label (GTK_LABEL (label), gdn_environment_info_get_key (item));
  g_object_unref (item);
}

static void
environment_key_unbind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_critical ("environment_key_unbind unimplemented");
}

static void
environment_key_teardown (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_critical ("environment_key_teardown unimplemented");
}

static void
environment_value_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
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
  GtkTreeListRow *list_row;
  GtkWidget *     expander;
  GtkWidget *     label;
  gpointer        item;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);

  label = gtk_list_item_get_child (list_item);
  gtk_label_set_label (GTK_LABEL (label),
                       gdn_environment_info_get_value (item));
  g_object_unref (item);
}

static void
environment_value_unbind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_critical ("environment_value_unbind unimplemented");
}

static void
environment_value_teardown (GtkListItemFactory *factory, GtkListItem *list_item)
{
  g_critical ("environment_value_teardown unimplemented");
}

static void
backtrace_setup (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, label);
}

static void
backtrace_teardown (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *label;

  label = gtk_list_item_get_child (list_item);
  if (label)
    g_object_unref (label);
}

static void
backtrace_bind (GtkListItemFactory *factory, GtkListItem *list_item)
{
  GtkLabel *     label;
  GdnThreadInfo *info;

  label = gtk_list_item_get_child (list_item);
  GObject *obj = gtk_list_item_get_item (list_item);
  if (list_item)
    {
      info = GDN_FRAME_INFO (obj);
      gtk_label_set_text (label, gdn_frame_info_get_name (info));
    }
  g_object_unref (obj);
}

static void
backtrace_activate (GtkListView *list, guint position, gpointer unused)
{
  // gboolean              gdn_lisp_switch_thread (GdnLisp *lisp, int thd_idx);
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

static char *
xread (int fd)
{
#define BLOCK_LEN 1024
#define MAX_LEN 10 * BLOCK_LEN
  ssize_t     len, total_len;
  char        buf[BLOCK_LEN];
  char        nullbuf[1] = { '\0' };
  char        badbuf[] = "■\n";
  GByteArray *gbarray;

  total_len = 0;
  gbarray = g_byte_array_new ();
  do
    {
      memset (buf, 0, BLOCK_LEN);
      len = read (fd, buf, BLOCK_LEN);
      if (len == -1)
        {
          g_critical ("reading from Lisp file descriptor failed: %s",
                      strerror (errno));
          break;
        }
      else if (len == 0)
        {
          g_critical ("EOF detected when reading from Lisp file descriptor");
          break;
        }
      else if (total_len <= MAX_LEN)
        {
          g_byte_array_append (gbarray, buf, len);
        }
      total_len += len;
    }
  while (len == BLOCK_LEN);
  if (total_len > MAX_LEN)
    {
      g_critical ("truncating excessive input read from Lisp file descriptor");
      g_byte_array_append (gbarray, badbuf, strlen (badbuf));
    }
  g_byte_array_append (gbarray, nullbuf, 1);
  return g_byte_array_free (gbarray, FALSE);
}
