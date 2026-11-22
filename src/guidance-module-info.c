/* guidance-module-info.c
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

#include "guidance-module-info.h"

struct _GdnModuleInfo
{
  GObject parent;

  gboolean    is_module;
  char *      module_name;
  char *      name;
  char *      file; /* NULL if unknown */
  int         line; /* 0 if unknown */
  gboolean    trappable;
  GListStore *children; /* of GdnModuleInfo, for a module */
};

G_DEFINE_TYPE (GdnModuleInfo, gdn_module_info, G_TYPE_OBJECT)

static void
gdn_module_info_finalize (GObject *object)
{
  GdnModuleInfo *self = GDN_MODULE_INFO (object);

  g_clear_pointer (&self->module_name, g_free);
  g_clear_pointer (&self->name, g_free);
  g_clear_pointer (&self->file, g_free);
  g_clear_object (&self->children);

  G_OBJECT_CLASS (gdn_module_info_parent_class)->finalize (object);
}

static void
gdn_module_info_class_init (GdnModuleInfoClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = gdn_module_info_finalize;
}

static void
gdn_module_info_init (G_GNUC_UNUSED GdnModuleInfo *self)
{
}

static char *
dup_nonempty (const char *s)
{
  return (s != NULL && *s != '\0') ? g_strdup (s) : NULL;
}

GdnModuleInfo *
gdn_module_info_new_module (const char *name, const char *file)
{
  GdnModuleInfo *self = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  self->is_module = TRUE;
  self->module_name = g_strdup (name);
  self->name = g_strdup (name);
  self->file = dup_nonempty (file);
  return self;
}

GdnModuleInfo *
gdn_module_info_new_procedure (const char *module_name,
                               const char *name,
                               const char *file,
                               int         line,
                               gboolean    trappable)
{
  GdnModuleInfo *self = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  self->is_module = FALSE;
  self->module_name = g_strdup (module_name);
  self->name = g_strdup (name);
  self->file = dup_nonempty (file);
  self->line = line;
  self->trappable = trappable;
  return self;
}

gboolean
gdn_module_info_is_module (GdnModuleInfo *info)
{
  return info->is_module;
}

gboolean
gdn_module_info_is_procedure (GdnModuleInfo *info)
{
  return !info->is_module;
}

const char *
gdn_module_info_get_name (GdnModuleInfo *info)
{
  return info->name;
}

const char *
gdn_module_info_get_module_name (GdnModuleInfo *info)
{
  return info->module_name;
}

const char *
gdn_module_info_get_file (GdnModuleInfo *info)
{
  return info->file;
}

int
gdn_module_info_get_line (GdnModuleInfo *info)
{
  return info->line;
}

gboolean
gdn_module_info_get_trappable (GdnModuleInfo *info)
{
  return info->trappable;
}

GListStore *
gdn_module_info_get_children (GdnModuleInfo *info)
{
  g_assert (info->is_module);
  if (info->children == NULL)
    info->children = g_list_store_new (GDN_MODULE_INFO_TYPE);
  return info->children;
}

gboolean
gdn_module_info_has_children (GdnModuleInfo *info)
{
  return info->children != NULL;
}
