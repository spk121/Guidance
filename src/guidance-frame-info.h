/* guidance-frame-info.h
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

#pragme once

#include <gtk/gtk.h>
#include <libguile.h>

G_BEGIN_DECLS

typedef enum yjd_frame_contents_t
{
  YJD_FRAME_CONTENTS_UNKNOWN = 0,
  YJD_FRAME_CONTENTS_FRAME,
  YJD_FRAME_CONTENTS_ARGUMENT,
  YJD_FRAME_CONTENTS_VARIABLE
} YjdFrameContents;

#define YJD_FRAME_CONTENTS_TYPE (yjd_frame_contents_get_type ())

#define GDN_FRAME_INFO_TYPE (yjd_frame_info_get_type ())
G_DECLARE_FINAL_TYPE (GdnFrameInfo, gdn_frame_info, GDN, FRAME_INFO, GObject);

// YjdFrameInfo *yjd_frame_info_new(const char *key, int index, gboolean enabled);
YjdFrameInfo *yjd_frame_info_new (YjdFrameContents contents, const char *name, GValue *value, const char *filename, int line, int col, GHashTable *arguments, GHashTable *locals);

/*
 * This is a callback of `GtkTreeListModelCreateModelFunc` type. This
 * procedure procedure creates a `GListModel` that stores the
 * variables for a frame. It for use by `gtk_tree_list_model_new` via
 * `gtk_tree_list_row_set_expanded`.
 *
 * @param item a GHashTable where key is a string and val is a GValue
 * @param user_data
 * @return A new GListModel of type YjdVariableInfo
 */

// GListModel *
// yjd_frame_info_create_variables_model(gpointer item, gpointer user_data){}

SCM gdn_frame_info_store_update (GListStore *store, SCM backtrace);

G_END_DECLS
