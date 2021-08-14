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

typedef enum gdn_module_category_t
{
  GDN_MODULE_CATEGORY_UNKNOWN = 0,
  GDN_MODULE_CATEGORY_LIBRARY,
  GDN_MODULE_CATEGORY_SITE,
  GDN_MODULE_CATEGORY_GLOBAL_SITE,
  GDN_MODULE_CATEGORY_PACKAGE_DATA,
  GDN_MODULE_CATEGORY_OTHER
} GdnModuleCategory;

#define GDN_MODULE_CATEGORY_TYPE (gdn_module_category_get_type ())

#define GDN_MODULE_INFO_TYPE (gdn_module_info_get_type ())
G_DECLARE_FINAL_TYPE (GdnModuleInfo, gdn_module_info, GDN, MODULE_INFO, GObject)

void gdn_module_info_store_append (GListStore *store, SCM entry);

G_END_DECLS
