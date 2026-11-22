/* guidance-xgtk.c
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

#include "guidance-xgtk.h"
#include <stdio.h>

void
xgtk_invoke_on_main (GSourceFunc func, gpointer data, GDestroyNotify notify)
{
  /* Idle sources of equal priority are dispatched in the order they
   * were added, which keeps the GUI updates in order. */
  g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, func, data, notify);
}

typedef struct
{
  GListStore *store;
  GPtrArray * items;
} StoreReplacement;

static gboolean
replace_all (gpointer user_data)
{
  StoreReplacement *r = user_data;
  g_list_store_splice (r->store, 0,
                       g_list_model_get_n_items (G_LIST_MODEL (r->store)),
                       r->items->pdata, r->items->len);
  return G_SOURCE_REMOVE;
}

static void
store_replacement_free (gpointer user_data)
{
  StoreReplacement *r = user_data;
  g_object_unref (r->store);
  g_ptr_array_unref (r->items);
  g_free (r);
}

void
xgtk_list_store_replace_all_on_main (GListStore *store, GPtrArray *items)
{
  g_assert (G_IS_LIST_STORE (store));
  g_assert_nonnull (items);

  StoreReplacement *r = g_new0 (StoreReplacement, 1);
  r->store = g_object_ref (store);
  r->items = items;
  xgtk_invoke_on_main (replace_all, r, store_replacement_free);
}

gboolean
xgtk_text_buffer_drop_old_text (GtkTextBuffer *buffer,
                                int            max_lines,
                                int            keep_lines,
                                int            max_chars,
                                int            keep_chars)
{
  g_assert (GTK_IS_TEXT_BUFFER (buffer));
  g_assert (keep_lines <= max_lines && keep_chars <= max_chars);

  int         lines = gtk_text_buffer_get_line_count (buffer);
  int         chars = gtk_text_buffer_get_char_count (buffer);
  GtkTextIter start, cut;

  if (lines <= max_lines && chars <= max_chars)
    return FALSE;

  /* The cut is the later of the two places that meet a limit. */
  gtk_text_buffer_get_start_iter (buffer, &cut);
  if (lines > keep_lines)
    gtk_text_iter_set_line (&cut, lines - keep_lines);
  if (chars - gtk_text_iter_get_offset (&cut) > keep_chars)
    {
      gtk_text_iter_set_offset (&cut, chars - keep_chars);
      /* Keep whole lines, unless that leaves nothing. */
      if (!gtk_text_iter_starts_line (&cut))
        {
          GtkTextIter next = cut;
          if (gtk_text_iter_forward_line (&next)
              && !gtk_text_iter_is_end (&next))
            cut = next;
        }
    }

  gtk_text_buffer_get_start_iter (buffer, &start);
  gtk_text_buffer_delete (buffer, &start, &cut);
  return TRUE;
}

void
xgtk_transcribe (const char *text)
{
  static FILE *   transcript = NULL;
  static gboolean checked = FALSE;

  if (!checked)
    {
      const char *path = g_getenv ("GUIDANCE_TRANSCRIPT");
      checked = TRUE;
      if (path != NULL && *path != '\0')
        transcript = fopen (path, "a");
    }
  if (transcript != NULL && text != NULL)
    {
      fputs (text, transcript);
      fflush (transcript);
    }
}
