/* guidance-environment-info.h
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

#define GDN_ENVIRONMENT_INFO_TYPE (gdn_environment_info_get_type ())
G_DECLARE_FINAL_TYPE (
    GdnEnvironmentInfo, gdn_environment_info, GDN, ENVIRONMENT_INFO, GObject)

struct _GdnEnvironmentInfo
{
  GObject parent;

  char *  key;
  char *  value;
  GSList *list;
};

GdnEnvironmentInfo *gdn_environment_info_new_from_scm (SCM info);

const char *gdn_environment_info_get_key (GdnEnvironmentInfo *info);
const char *gdn_environment_info_get_value (GdnEnvironmentInfo *info);
GListStore *gdn_environment_info_get_child_model (GdnEnvironmentInfo *info);
gboolean    gdn_environment_info_equal (GdnEnvironmentInfo *a,
                                        GdnEnvironmentInfo *b);
gint        gdn_environment_info_compare (GdnEnvironmentInfo *a,
                                          GdnEnvironmentInfo *b);

G_END_DECLS
