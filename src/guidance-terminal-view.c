/* guidance-terminal-view.c
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

#include "guidance-terminal-view.h"
#include <glib-unix.h>

struct _GdnTerminalView
{
  GtkBox parent_instance;

  GList *        history;
  GList *        history_cur;
  GtkTextView *  text_view;
  GtkTextBuffer *text_buffer;
  GtkLabel *     prompt_label;
  GtkEntry *     input_entry;
  guint          input_source;
  guint          prompt_source;
  guint          error_source;
  int            output_fd;
};

G_DEFINE_TYPE (GdnTerminalView, gdn_terminal_view, GTK_TYPE_BOX)

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
static void activate_terminal_entry (GtkEntry *entry, gpointer user_data);

static gboolean
poll_terminal_text (gint fd, GIOCondition condition, gpointer user_data);
static gboolean
poll_terminal_prompt (gint fd, GIOCondition condition, gpointer user_data);
static gboolean
poll_terminal_error (gint fd, GIOCondition condition, gpointer user_data);

static char *xread (int fd);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_terminal_view_class_init (GdnTerminalViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/terminal-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnTerminalView, x)

  BIND (text_view);
  BIND (text_buffer);
  BIND (prompt_label);
  BIND (input_entry);
#undef BIND
}

static void
gdn_terminal_view_init (GdnTerminalView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  self->history = NULL;
  self->history_cur = NULL;
  self->output_fd = STDOUT_FILENO;

  g_signal_connect (self->input_entry, "activate",
                    G_CALLBACK (activate_terminal_entry), self);
}

////////////////////////////////////////////////////////////////
// METHODS
////////////////////////////////////////////////////////////////

void
gdn_terminal_view_connect_ports (GdnTerminalView *self,
                                 int              input_fd,
                                 int              prompt_fd,
                                 int              error_fd,
                                 int              output_fd)
{
  self->input_source = g_unix_fd_add_full (
      G_PRIORITY_DEFAULT, input_fd, G_IO_IN, poll_terminal_text, self, NULL);
  self->prompt_source = g_unix_fd_add_full (
      G_PRIORITY_DEFAULT, prompt_fd, G_IO_IN, poll_terminal_prompt, self, NULL);
  self->error_source = g_unix_fd_add_full (
      G_PRIORITY_DEFAULT, error_fd, G_IO_IN, poll_terminal_error, self, NULL);
  self->output_fd = output_fd;
}

void
gdn_terminal_view_disconnect_lisp_ports (GdnTerminalView *self)
{
  g_source_remove (self->input_source);
  self->input_source = 0;
  g_source_remove (self->prompt_source);
  self->prompt_source = 0;
  g_source_remove (self->error_source);
  self->error_source = 0;
  self->output_fd = STDOUT_FILENO;
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

/* The operater has hit enter on the GtkEntry in the Terminal widget.
 * Send that text to the Lisp interpreter. */
static void
activate_terminal_entry (GtkEntry *entry, gpointer user_data)
{
  g_assert_cmpuint (G_OBJECT_TYPE (entry), ==, GTK_TYPE_ENTRY);
  g_assert_nonnull (user_data);
  g_assert_cmpuint (G_OBJECT_TYPE (user_data), ==, GDN_TYPE_TERMINAL_VIEW);

  const char *text;
  GdnTerminalView *self = user_data;

  text = gtk_entry_buffer_get_text (gtk_entry_get_buffer (entry));
  int bytes_written = write (self->output_fd, text, strlen (text));
  if (bytes_written == -1)
    g_critical ("Could not push text to the Guile interpreter");
  if (write (self->output_fd, "\n", 1) == -1)
    g_critical ("Could not push newline to the Guile interpreter");
  fsync (self->output_fd);

  /* Write the prompt and the text from the input GtkEntry box onto the main
   * output widget */
  GtkTextView *  view = self->text_view;
  GtkTextBuffer *text_buffer = gtk_text_view_get_buffer (view);
  GtkTextIter    iter_start, iter_end;
  GtkTextMark *  mark_start;
  const char *   prompt = gtk_label_get_text (self->prompt_label);

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
  gtk_entry_buffer_set_text (gtk_entry_get_buffer (self->input_entry), "", 0);
}

gboolean
poll_terminal_text (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnTerminalView *self = user_data;

  GtkTextView *  view = self->text_view;
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
poll_terminal_error (gint fd, GIOCondition condition, gpointer user_data)
{
  GdnTerminalView *self = GDN_TERMINAL_VIEW (user_data);
  GtkTextView *    view = self->text_view;
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
poll_terminal_prompt (gint fd, GIOCondition condition, gpointer user_data)
{
#define MAX_PROMPT_CODEPOINTS 80
#define MAX_PROMPT_BYTES (MAX_PROMPT_CODEPOINTS * 3)

  GdnTerminalView *self = GDN_TERMINAL_VIEW (user_data);
  char *buf;

  /* Clear out the old label */
  gtk_label_set_text (self->prompt_label, "");

  /* Read from the port */
  buf = xread (fd);

  /* How big a prompt should we handle? */
  glong u8_len;
  u8_len = g_utf8_strlen (buf, MAX_PROMPT_BYTES);
  if (u8_len > MAX_PROMPT_CODEPOINTS)
    {
      gchar *substr = g_utf8_substring (buf, 0, MAX_PROMPT_CODEPOINTS);
      gtk_label_set_text (self->prompt_label, substr);
      g_free (substr);
    }
  else if (u8_len > 0)
    gtk_label_set_text (self->prompt_label, buf);

  g_free (buf);

  return TRUE;
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static char *
xread (int fd)
{
#define BLOCK_LEN 1024
#define MAX_LEN 10 * BLOCK_LEN
  ssize_t     len, total_len;
  guint8      buf[BLOCK_LEN];
  guint8      nullbuf[1] = { '\0' };
  guint8      badbuf[] = "■\n";
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
      g_byte_array_append (gbarray, badbuf, sizeof (badbuf));
    }
  g_byte_array_append (gbarray, nullbuf, 1);
  return (char *) g_byte_array_free (gbarray, FALSE);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////
