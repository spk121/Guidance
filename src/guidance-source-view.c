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

static GtkTextView *_view;
static GtkLabel *   _label;
static GHashTable * _contents = (GHashTable *) NULL;
static GSList *     _prefixes = (GSList *) NULL;
static char *       _rel_path = (char *) NULL;

void
gdn_source_view_init (GtkTextView *view, GtkLabel *label)
{
  GtkTextBuffer *buf;

  _view = g_object_ref (view);
  _label = g_object_ref (label);

  buf = gtk_text_view_get_buffer (_view);
  gtk_text_buffer_create_tag (buf, "line-tag", "background", "yellow", NULL);
  gtk_text_buffer_create_tag (buf, "pos-tag", "background", "red", NULL);
}

void
gdn_source_view_set_paths (char **paths)
{
  if (_prefixes != (GSList *) NULL)
    g_slist_free_full (_prefixes, g_free);
  _prefixes = (GSList *) NULL;
  int i = 0;
  while (paths[i] != NULL)
    {
      _prefixes = g_slist_append (_prefixes, g_strdup (paths[i]));
      i++;
    }
  g_strfreev (paths);
}

char *
find_file (const char *rel_path)
{
  GSList *prefix = _prefixes;
  char *  abs_path;

  if (g_file_test (rel_path, G_FILE_TEST_IS_REGULAR))
    return rel_path;

  while (prefix != NULL)
    {
      abs_path = g_build_filename (prefix->data, rel_path, NULL);
      if (g_file_test (abs_path, G_FILE_TEST_IS_REGULAR))
        return abs_path;

      prefix = prefix->next;
    }
  return NULL;
}

static gboolean
scroll_onscreen (gpointer user_data)
{
  GtkTextBuffer *buf;
  GtkTextMark *  tmp_mark = GTK_TEXT_MARK (user_data);
  buf = gtk_text_view_get_buffer (_view);
  gtk_text_view_scroll_mark_onscreen (_view, tmp_mark);
  // gtk_text_buffer_delete_mark(buf, tmp_mark);
  return G_SOURCE_REMOVE;
}

gboolean
gdn_source_view_show_location (const char *rel_path, int line, int col)
{
  GtkTextBuffer *buf = NULL;

  // Different coordinate systems between GtkTextBuffer and Guile
  line--;
  col--;

  if (_contents == NULL)
    _contents = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  // Load up the file unless it is already loaded
  if (g_strcmp0 (rel_path, _rel_path) != 0)
    {
      gchar *text = NULL;
      gsize  len = 0;

      if (!(text = g_hash_table_lookup (_contents, rel_path)))
        {
          char *   abs_path;
          gboolean ret;

          abs_path = find_file (rel_path);
          if (!abs_path)
            return FALSE;
          ret = g_file_get_contents (abs_path, &text, &len, NULL);
          if (!ret)
            return FALSE;
          g_hash_table_insert (_contents, rel_path, text);
        }
      buf = gtk_text_view_get_buffer (_view);
      gtk_text_buffer_set_text (buf, text, -1);
    }

  buf = gtk_text_view_get_buffer (_view);

  g_free (_rel_path);
  _rel_path = g_strdup (rel_path);

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
  GtkTextMark *tmp_mark =
      gtk_text_buffer_create_mark (buf, NULL, &start_pos, FALSE);
  g_idle_add (scroll_onscreen, tmp_mark);

  gtk_label_set_text (_label, rel_path);

  return TRUE;
}

static SCM
scm_show_location (SCM sfname, SCM sline, SCM scol)
{
  SCM_ASSERT_TYPE (scm_is_string (sfname), sfname, SCM_ARG1,
                   "gdn-show-location", "string");
  SCM_ASSERT_TYPE (scm_is_exact_integer (sline), sline, SCM_ARG2,
                   "gdn-show-location", "integer");
  SCM_ASSERT_TYPE (scm_is_exact_integer (scol), scol, SCM_ARG3,
                   "gdn-show-location", "integer");
  char *   fname = scm_to_utf8_string (sfname);
  gboolean ret;
  ret = gdn_source_view_show_location (fname, scm_to_int (sline),
                                       scm_to_int (scol));
  free (fname);
  return scm_from_bool (ret);
}

void
gdn_source_view_guile_init (void)
{
  scm_c_define_gsubr ("gdn-show-location", 3, 0, 0, scm_show_location);
}
