/* guidance-binding-info.h
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

#define GDN_BINDING_INFO_TYPE (gdn_binding_info_get_type ())
G_DECLARE_FINAL_TYPE (
    GdnBindingInfo, gdn_binding_info, GDN, BINDING_INFO, GObject)

void        gdn_binding_info_update_all (SCM all_bindings);
GListStore *gdn_binding_info_get_list_store (void);
gboolean    gdn_binding_info_get_argument (GdnBindingInfo *self);
const char *gdn_binding_info_get_name (GdnBindingInfo *self);
const char *gdn_binding_info_get_representation (GdnBindingInfo *self);
const char *gdn_binding_info_get_value (GdnBindingInfo *self);
const char *gdn_binding_info_get_extra (GdnBindingInfo *self);
G_END_DECLS
