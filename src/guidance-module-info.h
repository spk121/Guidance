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

G_BEGIN_DECLS

/* An entry on the Modules page: either a module, or a procedure in a
 * module.  It holds only plain data, copied from the REPL thread, so
 * the GTK thread never touches Guile objects. */

#define GDN_MODULE_INFO_TYPE (gdn_module_info_get_type ())
G_DECLARE_FINAL_TYPE (GdnModuleInfo, gdn_module_info, GDN, MODULE_INFO, GObject)

GdnModuleInfo *gdn_module_info_new_module (const char *name, const char *file);
GdnModuleInfo *gdn_module_info_new_procedure (const char *module_name,
                                              const char *name,
                                              const char *file,
                                              int         line,
                                              gboolean    trappable);

gboolean    gdn_module_info_is_module (GdnModuleInfo *info);
gboolean    gdn_module_info_is_procedure (GdnModuleInfo *info);
const char *gdn_module_info_get_name (GdnModuleInfo *info);
/* For a procedure, the name of its module; for a module, its name. */
const char *gdn_module_info_get_module_name (GdnModuleInfo *info);
/* The absolute path of the source file, or NULL if unknown. */
const char *gdn_module_info_get_file (GdnModuleInfo *info);
/* The 1-based line of a procedure's source, or 0 if unknown. */
int         gdn_module_info_get_line (GdnModuleInfo *info);
gboolean    gdn_module_info_get_trappable (GdnModuleInfo *info);

/* A module's procedures.  The store is created empty the first time
 * this is called, and filled when the REPL sends the procedures. */
GListStore *gdn_module_info_get_children (GdnModuleInfo *info);
gboolean    gdn_module_info_has_children (GdnModuleInfo *info);

G_END_DECLS
