/* guidance-module-info.h
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
#include <libguile.h>

G_BEGIN_DECLS

#define GDN_MODULE_INFO_TYPE (gdn_module_info_get_type ())
G_DECLARE_FINAL_TYPE (GdnModuleInfo, gdn_module_info, GDN, MODULE_INFO, GObject)

void gdn_module_info_guile_init (void);

GtkTreeListModel *gdn_module_info_get_tree_model (void);
gboolean          gdn_module_info_is_top_level (GdnModuleInfo *info);
gboolean          gdn_module_info_is_module (GdnModuleInfo *info);
gboolean          gdn_module_info_is_procedure (GdnModuleInfo *info);
const char *      gdn_module_info_get_name (GdnModuleInfo *info);
SCM               gdn_module_info_get_procedure (GdnModuleInfo *info);
char *            gdn_module_info_get_abs_path (GdnModuleInfo *info);

G_END_DECLS
