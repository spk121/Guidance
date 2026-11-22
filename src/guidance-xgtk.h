/* guidance-xgtk.h
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

#pragma once

#include <gtk/gtk.h>

G_BEGIN_DECLS

/* Schedules FUNC (DATA) to run once on the GTK main thread.  NOTIFY,
 * if not NULL, is called on DATA afterwards.  Safe to call from any
 * thread.  Calls made in order are run in order. */
void xgtk_invoke_on_main (GSourceFunc func, gpointer data,
                          GDestroyNotify notify);

/* Replaces the entire contents of STORE with the objects in ITEMS, on
 * the GTK main thread.  Takes ownership of ITEMS, which should own
 * its elements (e.g. created with g_ptr_array_new_with_free_func
 * (g_object_unref)).  Safe to call from any thread. */
void xgtk_list_store_replace_all_on_main (GListStore *store,
                                          GPtrArray * items);

/* Keeps BUFFER from growing without limit.  If it has more than
 * MAX_LINES lines or MAX_CHARS characters, deletes its oldest text, in
 * whole lines where it can, until it has at most KEEP_LINES lines and
 * KEEP_CHARS characters.  Returns TRUE if it deleted anything.  GTK
 * thread only. */
gboolean xgtk_text_buffer_drop_old_text (GtkTextBuffer *buffer,
                                         int            max_lines,
                                         int            keep_lines,
                                         int            max_chars,
                                         int            keep_chars);

/* When the environment variable GUIDANCE_TRANSCRIPT names a file,
 * appends TEXT to it.  The tests use this to see what Guidance shows.
 * GTK thread only. */
void xgtk_transcribe (const char *text);
G_END_DECLS
