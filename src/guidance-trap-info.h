/* guidance-trap-info.h
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

#define GDN_TRAP_INFO_TYPE (gdn_trap_info_get_type ())
G_DECLARE_FINAL_TYPE (GdnTrapInfo, gdn_trap_info, GDN, TRAP_INFO, GObject)

void gdn_trap_info_store_update (GListStore *store, int trap_cur);
void gdn_trap_info_guile_init (void);
GListStore *gdn_trap_info_get_model (void);
void        gdn_trap_info_add_proc_trap_async (SCM proc);
int         gdn_trap_info_get_index (GdnTrapInfo *info);
const char *gdn_trap_info_get_name (GdnTrapInfo *info);
gboolean *  gdn_trap_info_get_active (GdnTrapInfo *info);

G_END_DECLS
