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
#include "guidance-lisp.h"
#include <gio/gunixinputstream.h>
#include <gio/gunixoutputstream.h>

struct _GdnApplicationWindow
{
  GtkApplicationWindow parent_instance;

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
static gboolean poll_terminal_error (GObject *pollable_stream,
                                     gpointer user_data);
static gboolean poll_terminal_prompt (GObject *pollable_stream,
                                      gpointer user_data);
static gboolean poll_terminal_text (GObject *pollable_stream,
                                    gpointer user_data);
static gboolean
poll_terminal_text2 (gint fd, GIOCondition condition, gpointer user_data);
static gboolean
poll_terminal_prompt2 (gint fd, GIOCondition condition, gpointer user_data);
static gboolean
poll_terminal_error2 (gint fd, GIOCondition condition, gpointer user_data);

static void add_simple_action (GdnApplicationWindow *self,
                               const char *          name,
                               GCallback             callback);
static void attach_stream (GdnApplicationWindow *self,
                           GPollableInputStream *stream,
                           GPollableSourceFunc   func);

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

  attach_stream (self, gdn_lisp_get_input_stream (self->lisp),
                 poll_terminal_text2);
  attach_stream (self, gdn_lisp_get_input_prompt_stream (self->lisp),
                 poll_terminal_prompt2);
  attach_stream (self, gdn_lisp_get_input_error_stream (self->lisp),
                 poll_terminal_error2);
  g_signal_connect (self->terminal_input_entry, "activate",
                    G_CALLBACK (activate_terminal_entry), self);
}

static void
gdn_application_window_init (GdnApplicationWindow *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  add_simple_action (self, "launch", G_CALLBACK (activate_launch));

  application_window_init_settings_tab (self);
  application_window_init_lisp_and_terminal_tab (self);
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
#if 0
  g_output_stream_write_all_async(gdn_lisp_get_output_stream(self->lisp),
                                  text,
                                  strlen(text),
                                  G_PRIORITY_DEFAULT,
                                  NULL,
                                  NULL,
                                  NULL);
#else
  int fd =
      g_unix_output_stream_get_fd (gdn_lisp_get_output_stream (self->lisp));
  int bytes_written = write (fd, text, strlen (text));
  if (bytes_written == -1)
    g_critical ("Could not push text to the Guile interpreter");
  if (write (fd, "\n", 1) == -1)
    g_critical ("Could not push newline to the Guile interpreter");
  fsync (fd);

#endif

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
poll_terminal_text (GObject *pollable_stream, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  GPollableInputStream *stream = G_POLLABLE_INPUT_STREAM (pollable_stream);
  g_critical ("POLL TERMINAL");
  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  char           buf[1024];
  gsize          bytes_read;

  // FIXME: figure out how to use the async version
  bytes_read =
      g_pollable_input_stream_read_nonblocking (stream, buf, 1024, NULL, NULL);

  /* Copy it over to the GtkTextView output widget. */
  gtk_text_buffer_insert_at_cursor (text_buffer, buf, bytes_read);
  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));

  return TRUE;
}

gboolean
poll_terminal_text2 (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  g_critical ("POLL TERMINAL");
  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  char           buf[1024];
  gsize          bytes_read;

  /* Get all the text from the read port. */
  bytes_read = read (fd, buf, 1024);

  /* Copy it over to the GtkTextView output widget. */
  gtk_text_buffer_insert_at_cursor (text_buffer, buf, bytes_read);
  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));

  return TRUE;
}

gboolean
poll_terminal_error (GObject *pollable_stream, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  GInputStream *        stream = G_INPUT_STREAM (pollable_stream);

  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  char           buf[1024];
  gsize          bytes_read;

  // FIXME: figure out how to use the async version
  g_input_stream_read_all (stream, buf, 1024, &bytes_read, NULL, NULL);

  GtkTextIter  iter_start, iter_end;
  GtkTextMark *mark_start;

  /* Store the 'before' location */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start,
                                    gtk_text_buffer_get_insert (text_buffer));
  mark_start =
      gtk_text_buffer_create_mark (text_buffer, NULL, &iter_start, TRUE);

  /* Copy it over to the GtkTextView output widget. */
  gtk_text_buffer_insert_at_cursor (text_buffer, buf, bytes_read);
  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));

  /* Change the presentation for the error text */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start, mark_start);
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_end,
                                    gtk_text_buffer_get_insert (text_buffer));
  gtk_text_buffer_apply_tag_by_name (text_buffer, "error", &iter_start,
                                     &iter_end);
  gtk_text_buffer_delete_mark (text_buffer, mark_start);
  return TRUE;
}

gboolean
poll_terminal_error2 (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);

  GtkTextView *  view = self->terminal_text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  char           buf[1024];
  gsize          bytes_read;

  /* Get all the text from the read port. */
  bytes_read = read (fd, buf, 1024);

  GtkTextIter  iter_start, iter_end;
  GtkTextMark *mark_start;

  /* Store the 'before' location */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start,
                                    gtk_text_buffer_get_insert (text_buffer));
  mark_start =
      gtk_text_buffer_create_mark (text_buffer, NULL, &iter_start, TRUE);

  /* Copy it over to the GtkTextView output widget. */
  gtk_text_buffer_insert_at_cursor (text_buffer, buf, bytes_read);
  gtk_text_view_scroll_mark_onscreen (view,
                                      gtk_text_buffer_get_insert (text_buffer));

  /* Change the presentation for the error text */
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_start, mark_start);
  gtk_text_buffer_get_iter_at_mark (text_buffer, &iter_end,
                                    gtk_text_buffer_get_insert (text_buffer));
  gtk_text_buffer_apply_tag_by_name (text_buffer, "error", &iter_start,
                                     &iter_end);
  gtk_text_buffer_delete_mark (text_buffer, mark_start);
  return TRUE;
}

gboolean
poll_terminal_prompt (GObject *pollable_stream, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  GInputStream *        stream = G_INPUT_STREAM (pollable_stream);
  gsize                 bytes_read;

  static char buf[1024];
  /* Get all the text from the read port. */
  g_input_stream_read_all (stream, buf, 1024, &bytes_read, NULL, NULL);

  /* Then copy it over to the prompt GtkLabel */
  gtk_label_set_text (self->terminal_prompt_label, buf);
  return TRUE;
}

gboolean
poll_terminal_prompt2 (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnApplicationWindow *self = GDN_APPLICATION_WINDOW (user_data);
  gsize                 bytes_read;

  static char buf[1024];
  /* Get all the text from the read port. */
  bytes_read = read (fd, buf, 1024);

  /* Then copy it over to the prompt GtkLabel */
  gtk_label_set_text (self->terminal_prompt_label, buf);
  return TRUE;
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
attach_stream (GdnApplicationWindow *self,
               GPollableInputStream *stream,
               GPollableSourceFunc   func)
{
#if ATTEMPT_1
  GSource *source;
  source = g_pollable_input_stream_create_source (stream, NULL);
  g_source_set_callback (source, G_SOURCE_FUNC (func), self, NULL);
  // g_source_attach(source, NULL);
  g_source_add_unix_fd (source, g_unix_input_stream_get_fd (stream), G_IO_IN);
#else
  g_unix_fd_add_full (G_PRIORITY_DEFAULT, g_unix_input_stream_get_fd (stream),
                      G_IO_IN, func, self, NULL);
#endif
}
