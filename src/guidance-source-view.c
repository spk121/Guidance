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
#include "guidance-lisp.h"

struct _GdnSourceView
{
  GtkBox       parent_instance;
  GtkTextView *source_view;
  GtkLabel *   source_label;
  GHashTable * contents;
  GSList *     prefixes;
  char *       rel_path;
  GtkTextMark *tmp_mark;
};

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
}

static void
gdn_source_view_init (GdnSourceView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));
  self->contents =
      g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  self->rel_path = NULL;

  char **paths = gdn_lisp_get_paths ();
  gdn_source_view_set_paths (self, (const char **) paths);
  g_strfreev (paths);

  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer (self->source_view);
  gtk_text_buffer_create_tag (buf, "line-tag", "background", "yellow", NULL);
  gtk_text_buffer_create_tag (buf, "pos-tag", "background", "red", NULL);
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

gboolean
gdn_source_view_show_location (GdnSourceView *self,
                               const char *   rel_path,
                               int            line,
                               int            col)
{
  g_assert (self->contents != NULL);
  GtkTextBuffer *buf = NULL;

  // Different coordinate systems between GtkTextBuffer and Guile
  line--;
  col--;

  // Load up the file unless it is already loaded
  if (g_strcmp0 (rel_path, self->rel_path) != 0)
    {
      gchar *text = NULL;
      gsize  len = 0;

      if (!(text = g_hash_table_lookup (self->contents, rel_path)))
        {
          char *   abs_path;
          gboolean ret;

          abs_path = find_file (self->prefixes, rel_path);
          if (!abs_path)
            return FALSE;
          ret = g_file_get_contents (abs_path, &text, &len, NULL);
          g_free (abs_path);
          if (!ret)
            return FALSE;
          g_hash_table_insert (self->contents, g_strdup (rel_path), text);
        }
      buf = gtk_text_view_get_buffer (self->source_view);
      gtk_text_buffer_set_text (buf, text, -1);
    }

  buf = gtk_text_view_get_buffer (self->source_view);

  g_free (self->rel_path);
  self->rel_path = g_strdup (rel_path);

  // Remove any old highlighting
  GtkTextIter start, end;
  gtk_text_buffer_get_bounds (buf, &start, &end);
  gtk_text_buffer_remove_all_tags (buf, &start, &end);

  // Highlight the current line and location
  GtkTextIter start_line, end_line;
  GtkTextIter start_pos, end_pos;
  gtk_text_buffer_get_iter_at_line_offset (buf, &start_line, line, 0);
  gtk_text_buffer_get_iter_at_line_offset (buf, &end_line, line + 1, 0);
  gtk_text_buffer_get_iter_at_line_offset (buf, &start_pos, line, col);
  gtk_text_buffer_get_iter_at_line_offset (buf, &end_pos, line, col + 1);

  gtk_text_buffer_apply_tag_by_name (buf, "line-tag", &start_line, &end_line);
  gtk_text_buffer_apply_tag_by_name (buf, "pos-tag", &start_pos, &end_pos);

  // Move the cursor to the current location
  self->tmp_mark = gtk_text_buffer_create_mark (buf, NULL, &start_pos, FALSE);
  g_idle_add (scroll_onscreen, self);

  gtk_label_set_text (self->source_label, rel_path);

  return TRUE;
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static gboolean
scroll_onscreen (gpointer user_data)
{
  GdnSourceView *self = user_data;
  gtk_text_view_scroll_mark_onscreen (self->source_view, self->tmp_mark);
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
    return g_strdup (rel_path);

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

  GdnSourceView *self = scm_foreign_object_ref (s_self, 0);
  char *         fname = scm_to_utf8_string (s_fname);
  gboolean ret;
  ret = gdn_source_view_show_location (self, fname, scm_to_int (s_line),
                                       scm_to_int (s_col));
  free (fname);
  return scm_from_bool (ret);
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
