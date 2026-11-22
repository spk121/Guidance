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
#include "guidance-xgtk.h"
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
  int            input_fd; /* The program's output */
  int            error_fd; /* The program's error output */
  /* Bytes read from each stream that end part-way through a UTF-8
   * character, kept until the rest arrives. */
  GByteArray *   input_pending;
  GByteArray *   prompt_pending;
  GByteArray *   error_pending;
  /* A prompt read so far, until the newline that ends it. */
  GString *      prompt_partial;
  GtkTextMark *  end_mark; /* Stays at the end of the text */
  guint          scroll_source;
  /* For the transcript: whether old text has been dropped, whether
   * the transcript says so yet, and whether it is at a line start. */
  gboolean       trimmed;
  gboolean       trim_noted;
  gboolean       at_line_start;
  int            line_chars; /* Characters in the last line of the text */
  gboolean       port_mode;
  gboolean       echo_mode;
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

static char *read_text (int fd, GByteArray *pending);
static void  append_text (GdnTerminalView *self,
                          const char *     text,
                          const char *     tag_name);
static void  drain_stream (GdnTerminalView *self,
                           int              fd,
                           GByteArray *     pending,
                           const char *     tag_name);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

enum
{
  ENTRY = 0,
  N_SIGNALS
};

static unsigned signals[N_SIGNALS];

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

  signals[ENTRY] =
      g_signal_new ("entry", G_TYPE_FROM_CLASS (klass),
                    G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, 0, NULL, NULL,
                    NULL, G_TYPE_NONE, 1, G_TYPE_STRING);
}

static void
gdn_terminal_view_init (GdnTerminalView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  self->history = NULL;
  self->history_cur = NULL;
  self->output_fd = STDOUT_FILENO;
  self->input_pending = g_byte_array_new ();
  self->prompt_pending = g_byte_array_new ();
  self->prompt_partial = g_string_new (NULL);
  self->error_pending = g_byte_array_new ();
  {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer (self->text_view);
    GtkTextIter    end;
    gtk_text_buffer_get_end_iter (buffer, &end);
    self->end_mark = gtk_text_buffer_create_mark (buffer, NULL, &end, FALSE);
  }
  self->scroll_source = 0;
  self->trimmed = FALSE;
  self->trim_noted = FALSE;
  self->at_line_start = TRUE;
  self->line_chars = 0;
  self->port_mode = TRUE;
  self->echo_mode = TRUE;

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
  /* The program's output is read when the GTK thread has nothing else
   * to do.  A program that prints without pause then cannot keep the
   * window from being drawn or from responding: it is held up, by its
   * full pseudoterminal, until its output has been read. */
  self->input_source =
      g_unix_fd_add_full (G_PRIORITY_DEFAULT_IDLE, input_fd, G_IO_IN,
                          poll_terminal_text, self, NULL);
  self->prompt_source =
      g_unix_fd_add_full (G_PRIORITY_DEFAULT_IDLE, prompt_fd, G_IO_IN,
                          poll_terminal_prompt, self, NULL);
  self->error_source =
      g_unix_fd_add_full (G_PRIORITY_DEFAULT_IDLE, error_fd, G_IO_IN,
                          poll_terminal_error, self, NULL);
  self->output_fd = output_fd;
  self->input_fd = input_fd;
  self->error_fd = error_fd;

  /* A stream that is ready when the GTK thread looks may have been
   * read by drain_stream by the time its handler runs.  The read must
   * then come back empty-handed rather than wait, which would stop the
   * GTK thread. */
  g_unix_set_fd_nonblocking (input_fd, TRUE, NULL);
  g_unix_set_fd_nonblocking (prompt_fd, TRUE, NULL);
  g_unix_set_fd_nonblocking (error_fd, TRUE, NULL);
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

void
gdn_terminal_view_focus_entry (GdnTerminalView *self)
{
  gtk_widget_grab_focus (GTK_WIDGET (self->input_entry));
}

void
gdn_terminal_view_set_port_mode (GdnTerminalView *self, gboolean flag)
{
  self->port_mode = flag;
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

  /* When debugging a program, input is handled by sending it to the
   * lisp interpreter.  But when running, we pretend that we are
   * stdin. */
  if (self->port_mode == FALSE)
    {
      g_signal_emit (self, signals[ENTRY], 0, text);
    }
  else
    {
      int bytes_written = write (self->output_fd, text, strlen (text));
      if (bytes_written == -1)
        g_critical ("Could not push text to the Guile interpreter");
      if (write (self->output_fd, "\n", 1) == -1)
        g_critical ("Could not push newline to the Guile interpreter");
      fsync (self->output_fd);
    }

  if (self->echo_mode)
    {
      /* Show the text from the input GtkEntry box in the main output
       * widget. */
      char *line = g_strconcat (text, "\n", NULL);
      append_text (self, line, "input");
      g_free (line);
    }

  /* Append the entry to the history */
  if (!self->history || strncmp (text, self->history->data, strlen (text)) != 0)
    self->history =
        g_list_prepend (self->history, strndup (text, strlen (text)));
  self->history_cur = self->history;

  /* Then clear the entry box. */
  gtk_entry_buffer_set_text (gtk_entry_get_buffer (self->input_entry), "", 0);
}

gboolean
poll_terminal_text (gint                       fd,
                    G_GNUC_UNUSED GIOCondition condition,
                    gpointer                   user_data)
{
  GdnTerminalView *self = user_data;
  char *           text = read_text (fd, self->input_pending);

  if (text == NULL)
    return G_SOURCE_REMOVE;
  append_text (self, text, NULL);
  g_free (text);
  return G_SOURCE_CONTINUE;
}

gboolean
poll_terminal_error (gint                       fd,
                     G_GNUC_UNUSED GIOCondition condition,
                     gpointer                   user_data)
{
  GdnTerminalView *self = GDN_TERMINAL_VIEW (user_data);
  char *           text = read_text (fd, self->error_pending);

  if (text == NULL)
    return G_SOURCE_REMOVE;
  append_text (self, text, "error");
  g_free (text);
  return G_SOURCE_CONTINUE;
}

gboolean
poll_terminal_prompt (gint                       fd,
                      G_GNUC_UNUSED GIOCondition condition,
                      gpointer                   user_data)
{
#define MAX_PROMPT_CODEPOINTS 80
#define MAX_PROMPT_BYTES (MAX_PROMPT_CODEPOINTS * 3)

  GdnTerminalView *self = GDN_TERMINAL_VIEW (user_data);
  char *buf;

  /* The REPL prints its prompt after the output that goes before it,
   * but by another stream.  Show that output first. */
  drain_stream (self, self->input_fd, self->input_pending, NULL);
  drain_stream (self, self->error_fd, self->error_pending, "error");

  /* Read from the port */
  buf = read_text (fd, self->prompt_pending);
  if (buf == NULL)
    return G_SOURCE_REMOVE;

  /* The REPL ends each prompt with a newline.  One read can hold more
   * than one prompt, or part of one: show the last complete one, and
   * keep what follows until its newline arrives. */
  g_string_append (self->prompt_partial, buf);
  g_free (buf);

  char *newline;
  while ((newline = strchr (self->prompt_partial->str, '\n')) != NULL)
    {
      char *prompt = g_strndup (self->prompt_partial->str,
                                newline - self->prompt_partial->str);
      /* The pseudoterminal turns the newline into a carriage return
       * and a newline. */
      g_strdelimit (prompt, "\r", '\0');
      g_string_erase (self->prompt_partial, 0,
                      newline - self->prompt_partial->str + 1);

      /* How big a prompt should we handle? */
      if (g_utf8_strlen (prompt, MAX_PROMPT_BYTES) > MAX_PROMPT_CODEPOINTS)
        {
          gchar *substr = g_utf8_substring (prompt, 0, MAX_PROMPT_CODEPOINTS);
          g_free (prompt);
          prompt = substr;
        }
      gtk_label_set_text (self->prompt_label, prompt);

      /* Prompts are shown in a label, not the text view, so mark them
       * in the transcript. */
      char *line = g_strdup_printf ("[prompt %s]\n", prompt);
      xgtk_transcribe (line);
      g_free (line);
      g_free (prompt);
    }

  return G_SOURCE_CONTINUE;
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

/* The terminal keeps this much text.  When it has more, the oldest is
 * dropped, in whole lines, down to the lower figure, so that it need
 * not be trimmed every time something is printed.  The limit in
 * characters is for output with few line breaks. */
#define MAX_LINES 10000
#define TRIMMED_LINES 9000
#define MAX_CHARS 2000000
#define TRIMMED_CHARS 1800000

/* A text view becomes very slow, and uses a great deal of memory, when
 * a single line is enormous.  Lines longer than this are shown broken
 * into lines of this length. */
#define MAX_LINE_CHARS 1000

/* Returns TEXT with a line break added wherever a line would become
 * longer than MAX_LINE_CHARS, or NULL if it needs none.  LINE_CHARS is
 * the length of the line that TEXT continues, and is updated. */
static char *
break_long_lines (const char *text, int *line_chars)
{
  GString *   broken = NULL;
  const char *piece = text;

  for (const char *p = text; *p != '\0'; p = g_utf8_next_char (p))
    {
      if (*p == '\n')
        *line_chars = 0;
      else if (*line_chars == MAX_LINE_CHARS)
        {
          if (broken == NULL)
            broken = g_string_new (NULL);
          g_string_append_len (broken, piece, p - piece);
          g_string_append_c (broken, '\n');
          piece = p;
          *line_chars = 1;
        }
      else
        (*line_chars)++;
    }
  if (broken == NULL)
    return NULL;
  g_string_append (broken, piece);
  return g_string_free (broken, FALSE);
}

/* Reads what is waiting on FD, which must be ready to read, and
 * returns it as valid, NUL-terminated UTF-8, to be freed with g_free.
 * Nothing is left out: whatever is still waiting keeps FD ready, and
 * is read by the next call.
 *
 * A read can end part-way through a UTF-8 character.  Those bytes are
 * kept in PENDING and returned with the rest of the character by a
 * later call.  Bytes that are not UTF-8 at all are replaced.
 *
 * Returns NULL if FD can no longer be read. */
static char *
read_text (int fd, GByteArray *pending)
{
#define READ_LEN 65536
  guint       old_len = pending->len;
  ssize_t     len;
  const char *data, *end;
  gsize       valid_len, rest_len;
  char *      text;

  g_byte_array_set_size (pending, old_len + READ_LEN);
  len = read (fd, pending->data + old_len, READ_LEN);
  if (len <= 0)
    {
      g_byte_array_set_size (pending, old_len);
      if (len == -1 && (errno == EINTR || errno == EAGAIN))
        return g_strdup ("");
      if (len == -1)
        g_critical ("reading from Lisp file descriptor failed: %s",
                    strerror (errno));
      else
        g_critical ("EOF detected when reading from Lisp file descriptor");
      return NULL;
    }
  g_byte_array_set_size (pending, old_len + len);

  /* The text is handled as C strings, which a NUL would cut short.
   * Make it a byte that is not UTF-8, to be replaced like any other. */
  for (guint i = old_len; i < pending->len; i++)
    if (pending->data[i] == '\0')
      pending->data[i] = 0xff;

  data = (const char *) pending->data;
  if (g_utf8_validate (data, pending->len, &end))
    {
      text = g_strndup (data, pending->len);
      g_byte_array_set_size (pending, 0);
      return text;
    }

  valid_len = end - data;
  rest_len = pending->len - valid_len;
  if (g_utf8_get_char_validated (end, rest_len) == (gunichar) -2)
    {
      /* The rest is the start of a character.  Keep it. */
      text = g_strndup (data, valid_len);
      g_byte_array_remove_range (pending, 0, valid_len);
      return text;
    }

  text = g_utf8_make_valid (data, pending->len);
  g_byte_array_set_size (pending, 0);
  return text;
}

/* Reads and shows what is waiting on FD now, without waiting for more.
 * It stops after a few megabytes, in case another thread of the
 * program is printing without end. */
static void
drain_stream (GdnTerminalView *self,
              int              fd,
              GByteArray *     pending,
              const char *     tag_name)
{
  for (int i = 0; i < 64; i++)
    {
      GPollFD poll_fd = { fd, G_IO_IN, 0 };
      char *  text;

      if (g_poll (&poll_fd, 1, 0) <= 0 || !(poll_fd.revents & G_IO_IN))
        break;
      text = read_text (fd, pending);
      if (text == NULL)
        break;
      append_text (self, text, tag_name);
      g_free (text);
    }
}

static gboolean
scroll_to_end (gpointer user_data)
{
  GdnTerminalView *self = user_data;

  self->scroll_source = 0;
  gtk_text_view_scroll_mark_onscreen (self->text_view, self->end_mark);
  return G_SOURCE_REMOVE;
}

/* Appends TEXT to the terminal, in the style of the tag TAG_NAME if it
 * is not NULL, drops the oldest text if there is now too much, and
 * shows the end. */
static void
append_text (GdnTerminalView *self, const char *text, const char *tag_name)
{
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (self->text_view);
  GtkTextIter    end;
  char *         broken;
  const char *   shown;

  if (*text == '\0')
    return;

  /* The transcript has the text as it came.  For the tests, it also
   * notes, once and between lines, that old text has been dropped. */
  if (self->trimmed && !self->trim_noted && self->at_line_start)
    {
      self->trim_noted = TRUE;
      xgtk_transcribe ("[old text dropped]\n");
    }
  xgtk_transcribe (text);
  self->at_line_start = g_str_has_suffix (text, "\n");
  broken = break_long_lines (text, &self->line_chars);
  shown = broken != NULL ? broken : text;

  /* At the end, wherever the operator's cursor or selection is. */
  gtk_text_buffer_get_end_iter (buffer, &end);
  if (tag_name != NULL)
    gtk_text_buffer_insert_with_tags_by_name (buffer, &end, shown, -1,
                                              tag_name, NULL);
  else
    gtk_text_buffer_insert (buffer, &end, shown, -1);
  g_free (broken);

  if (xgtk_text_buffer_drop_old_text (buffer, MAX_LINES, TRIMMED_LINES,
                                      MAX_CHARS, TRIMMED_CHARS))
    self->trimmed = TRUE;

  /* Show the end.  The view scrolls once it has laid out the new
   * text, which it does when idle, not for each piece of output. */
  gtk_text_view_scroll_to_mark (self->text_view, self->end_mark, 0.0, TRUE,
                                0.0, 1.0);

  /* After a lot of output at once, that can stop short of the end,
   * because the view only estimates the height of text that it has
   * not laid out yet.  Go to the end again once things are quiet. */
  if (self->scroll_source == 0)
    self->scroll_source =
        g_idle_add_full (G_PRIORITY_LOW, scroll_to_end, g_object_ref (self),
                         g_object_unref);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////
