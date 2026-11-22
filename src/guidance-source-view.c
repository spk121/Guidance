/* guidance-source-view.c
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

#include "guidance-source-view.h"
#include "guidance-xgtk.h"
#include "guidance-lisp.h"

struct _GdnSourceView
{
  GtkBox       parent_instance;
  GtkTextView *source_view;
  GtkLabel *   source_label;
  GHashTable * contents;
  GSList *     prefixes;
  char *       rel_path; /* The file as it was named */
  char *       abs_path; /* The file shown, or NULL */
  GtkTextMark *location_mark;
};

enum
{
  BREAKPOINT = 0,
  N_SIGNALS
};

static unsigned signals[N_SIGNALS];

G_DEFINE_TYPE (GdnSourceView, gdn_source_view, GTK_TYPE_BOX)

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static char *   find_file (GSList *prefixes, const char *rel_path);
static gboolean scroll_onscreen (gpointer user_data);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_source_view_class_init (GdnSourceViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/source-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnSourceView, x)

  BIND (source_view);
  BIND (source_label);
#undef BIND

  /* The operator asked for a breakpoint at (FILE, LINE). */
  signals[BREAKPOINT] = g_signal_new ("breakpoint", G_TYPE_FROM_CLASS (klass),
                                      G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                                      G_TYPE_NONE, 2, G_TYPE_STRING,
                                      G_TYPE_INT);
}

static void
set_breakpoint_activated (G_GNUC_UNUSED GSimpleAction *action,
                          G_GNUC_UNUSED GVariant *     parameter,
                          gpointer                     user_data)
{
  gdn_source_view_request_breakpoint (GDN_SOURCE_VIEW (user_data));
}

static void
gdn_source_view_init (GdnSourceView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));
  self->contents =
      g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  self->rel_path = NULL;
  self->abs_path = NULL;

  char **paths = gdn_lisp_get_paths ();
  gdn_source_view_set_paths (self, (const char **) paths);
  g_strfreev (paths);

  GtkTextBuffer *buf;
  GtkTextIter    start;
  buf = gtk_text_view_get_buffer (self->source_view);
  gtk_text_buffer_create_tag (buf, "line-tag", "background", "yellow", NULL);
  gtk_text_buffer_create_tag (buf, "pos-tag", "background", "red", NULL);
  gtk_text_buffer_get_start_iter (buf, &start);
  self->location_mark = gtk_text_buffer_create_mark (buf, NULL, &start, TRUE);

  /* Add "Set Breakpoint Here" to the text view's context menu. */
  GSimpleActionGroup *group = g_simple_action_group_new ();
  GSimpleAction *     action = g_simple_action_new ("set-breakpoint", NULL);
  g_signal_connect (action, "activate", G_CALLBACK (set_breakpoint_activated),
                    self);
  g_action_map_add_action (G_ACTION_MAP (group), G_ACTION (action));
  g_object_unref (action);
  gtk_widget_insert_action_group (GTK_WIDGET (self), "source",
                                  G_ACTION_GROUP (group));
  g_object_unref (group);

  GMenu *menu = g_menu_new ();
  g_menu_append (menu, "Set Breakpoint Here", "source.set-breakpoint");
  gtk_text_view_set_extra_menu (self->source_view, G_MENU_MODEL (menu));
  g_object_unref (menu);
}

gboolean
gdn_source_view_get_cursor_location (GdnSourceView *self,
                                     const char **  file,
                                     int *          line)
{
  GtkTextBuffer *buf = gtk_text_view_get_buffer (self->source_view);
  GtkTextIter    iter;

  if (self->abs_path == NULL)
    return FALSE;
  gtk_text_buffer_get_iter_at_mark (buf, &iter,
                                    gtk_text_buffer_get_insert (buf));
  *file = self->abs_path;
  *line = gtk_text_iter_get_line (&iter) + 1;
  return TRUE;
}

void
gdn_source_view_request_breakpoint (GdnSourceView *self)
{
  const char *file;
  int         line;

  if (gdn_source_view_get_cursor_location (self, &file, &line))
    g_signal_emit (self, signals[BREAKPOINT], 0, file, line);
}

////////////////////////////////////////////////////////////////
// METHODS
////////////////////////////////////////////////////////////////

void
gdn_source_view_set_paths (GdnSourceView *self, const char **paths)
{
  if (self->prefixes != (GSList *) NULL)
    g_slist_free_full (self->prefixes, g_free);
  self->prefixes = (GSList *) NULL;
  int i = 0;
  while (paths[i] != NULL)
    {
      self->prefixes = g_slist_append (self->prefixes, g_strdup (paths[i]));
      i++;
    }
}

/* Shows LINE (1-based) and COL (0-based) of the file REL_PATH, which
 * may be absolute or relative to Guile's load path.  A LINE of 0 shows
 * the top of the file without highlighting.  Returns FALSE if the file
 * can't be read. */
gboolean
gdn_source_view_show_location (GdnSourceView *self,
                               const char *   rel_path,
                               int            line,
                               int            col)
{
  g_assert (self->contents != NULL);
  GtkTextBuffer *buf = gtk_text_view_get_buffer (self->source_view);

  if (rel_path == NULL || *rel_path == '\0')
    return FALSE;

  /* Load up the file unless it is already shown. */
  if (g_strcmp0 (rel_path, self->rel_path) != 0)
    {
      char *abs_path = find_file (self->prefixes, rel_path);
      char *text;

      if (abs_path == NULL)
        return FALSE;
      if (!(text = g_hash_table_lookup (self->contents, abs_path)))
        {
          if (!g_file_get_contents (abs_path, &text, NULL, NULL))
            {
              g_free (abs_path);
              return FALSE;
            }
          g_hash_table_insert (self->contents, g_strdup (abs_path), text);
        }
      gtk_text_buffer_set_text (buf, text, -1);
      g_free (self->rel_path);
      self->rel_path = g_strdup (rel_path);
      g_free (self->abs_path);
      self->abs_path = abs_path;
      gtk_label_set_text (self->source_label, abs_path);
    }

  /* Remove any old highlighting */
  GtkTextIter start, end;
  gtk_text_buffer_get_bounds (buf, &start, &end);
  gtk_text_buffer_remove_all_tags (buf, &start, &end);

  /* Guile's lines are 1-based and its columns 0-based; GtkTextBuffer
   * counts both from 0. */
  if (line > 0)
    {
      GtkTextIter start_line, end_line, start_pos, end_pos;

      gtk_text_buffer_get_iter_at_line (buf, &start_line, line - 1);
      end_line = start_line;
      gtk_text_iter_forward_line (&end_line);
      start_pos = start_line;
      if (col > 0 && col < gtk_text_iter_get_chars_in_line (&start_line))
        gtk_text_iter_set_line_offset (&start_pos, col);
      end_pos = start_pos;
      gtk_text_iter_forward_char (&end_pos);

      gtk_text_buffer_apply_tag_by_name (buf, "line-tag", &start_line,
                                         &end_line);
      gtk_text_buffer_apply_tag_by_name (buf, "pos-tag", &start_pos, &end_pos);
      gtk_text_buffer_place_cursor (buf, &start_pos);
      gtk_text_buffer_move_mark (buf, self->location_mark, &start_pos);
    }
  else
    {
      gtk_text_buffer_place_cursor (buf, &start);
      gtk_text_buffer_move_mark (buf, self->location_mark, &start);
    }

  /* Scroll once the new text has been laid out. */
  g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, scroll_onscreen, g_object_ref (self),
                   g_object_unref);
  return TRUE;
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static gboolean
scroll_onscreen (gpointer user_data)
{
  GdnSourceView *self = user_data;
  gtk_text_view_scroll_to_mark (self->source_view, self->location_mark, 0.0,
                                TRUE, 0.0, 0.3);
  return G_SOURCE_REMOVE;
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static char *
find_file (GSList *_prefixes, const char *rel_path)
{
  GSList *prefix = _prefixes;
  char *  abs_path;

  if (g_file_test (rel_path, G_FILE_TEST_IS_REGULAR))
    return g_canonicalize_filename (rel_path, NULL);

  while (prefix != NULL)
    {
      abs_path = g_build_filename (prefix->data, rel_path, NULL);
      if (g_file_test (abs_path, G_FILE_TEST_IS_REGULAR))
        return abs_path;

      prefix = prefix->next;
    }
  return NULL;
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

static SCM scm_source_view_type;

SCM
gdn_source_view_to_scm (GdnSourceView *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_SOURCE_VIEW);
  return scm_make_foreign_object_1 (scm_source_view_type, self);
}

typedef struct
{
  GdnSourceView *view;
  char *         filename;
  int            line;
  int            column;
} SourceLocation;

static gboolean
show_location_on_main (gpointer data)
{
  SourceLocation *loc = data;
  gdn_source_view_show_location (loc->view, loc->filename, loc->line,
                                 loc->column);
  return G_SOURCE_REMOVE;
}

static void
source_location_free (gpointer data)
{
  SourceLocation *loc = data;
  g_object_unref (loc->view);
  g_free (loc->filename);
  g_free (loc);
}

static SCM
scm_source_view_show_location (SCM s_self, SCM s_fname, SCM s_line, SCM s_col)
{
  scm_assert_foreign_object_type (scm_source_view_type, s_self);
  SCM_ASSERT_TYPE (scm_is_string (s_fname), s_fname, SCM_ARG2,
                   "gdn-source-view-show-location", "string");
  SCM_ASSERT_TYPE (scm_is_exact_integer (s_line), s_line, SCM_ARG3,
                   "gdn-source-view-show-location", "integer");
  SCM_ASSERT_TYPE (scm_is_exact_integer (s_col), s_col, SCM_ARG4,
                   "gdn-source-view-show-location", "integer");

  /* Called from the Guile thread, so the GTK work is sent to the GTK
   * thread. */
  SourceLocation *loc = g_new0 (SourceLocation, 1);
  char *          fname = scm_to_utf8_string (s_fname);
  loc->view = g_object_ref (scm_foreign_object_ref (s_self, 0));
  loc->filename = g_strdup (fname);
  loc->line = scm_to_int (s_line);
  loc->column = scm_to_int (s_col);
  free (fname);
  xgtk_invoke_on_main (show_location_on_main, loc, source_location_free);
  return SCM_UNSPECIFIED;
}

void
gdn_source_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-source-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_source_view_type = scm_make_foreign_object_type (name, slots, NULL);

  scm_c_define_gsubr ("gdn-source-view-show-location", 4, 0, 0,
                      scm_source_view_show_location);
}
