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

GType
gdn_module_category_get_type (void)
{
  static gsize gdn_module_category_type = 0;

  if (g_once_init_enter (&gdn_module_category_type))
    {
      static const GEnumValue values[] = {
        { GDN_MODULE_CATEGORY_UNKNOWN, "GDN_MODULE_CATEGORY_UNKNOWN",
          "unknown" },
        { GDN_MODULE_CATEGORY_LIBRARY, "GDN_MODULE_CATEGORY_LIBRARY",
          "library" },
        { GDN_MODULE_CATEGORY_SITE, "GDN_MODULE_CATEGORY_SITE", "site" },
        { GDN_MODULE_CATEGORY_GLOBAL_SITE, "GDN_MODULE_CATEGORY_SITE",
          "global-site" },
        { GDN_MODULE_CATEGORY_PACKAGE_DATA, "GDN_MODULE_CATEGORY_PACKAGE_DATA",
          "package-data" },
        { GDN_MODULE_CATEGORY_OTHER, "GDN_MODULE_CATEGORY_OTHER", "other" },
        { 0, NULL, NULL }
      };
      GType type;

      type = g_enum_register_static ("GdnModuleCategory", values);

      g_once_init_leave (&gdn_module_category_type, type);
    }
  return gdn_module_category_type;
}

struct _GdnModuleInfo
{
  GObject parent;

  char *            name;
  char *            short_path;
  char *            abs_path;
  GdnModuleCategory category;
};

G_DEFINE_TYPE (GdnModuleInfo, gdn_module_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_NAME,
  PROP_SHORT_PATH,
  PROP_ABS_PATH,
  PROP_CATEGORY,
  N_PROPS
};
static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static void
gdn_module_info_get_property (GObject *    object,
                              unsigned int property_id,
                              GValue *     value,
                              GParamSpec * pspec)
{
  GdnModuleInfo *self = GDN_MODULE_INFO (object);

  switch (property_id)
    {
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_SHORT_PATH:
      g_value_set_pointer (value, self->short_path);
      break;
    case PROP_ABS_PATH:
      g_value_set_pointer (value, self->abs_path);
      break;
    case PROP_CATEGORY:
      g_value_set_enum (value, self->category);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_module_info_finalize (GObject *object)
{
  GdnModuleInfo *self = GDN_MODULE_INFO (object);
  free (self->name);
  self->name = NULL;
  free (self->short_path);
  self->short_path = NULL;
  free (self->abs_path);
  self->abs_path = NULL;
  self->category = GDN_MODULE_CATEGORY_UNKNOWN;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_module_info_parent_class)->finalize (object);
}

static void
gdn_module_info_class_init (GdnModuleInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_module_info_finalize;
  gobj_class->get_property = gdn_module_info_get_property;

  properties[PROP_NAME] = g_param_spec_string (
      "name", "name", "A name of some module", NULL, G_PARAM_READABLE);
  properties[PROP_SHORT_PATH] = g_param_spec_string (
      "short-path", "short-path", "The relative path of an module", NULL,
      G_PARAM_READABLE);
  properties[PROP_ABS_PATH] = g_param_spec_string (
      "abs-path", "abs-path", "The absolute path of an module", NULL,
      G_PARAM_READABLE);
  properties[PROP_CATEGORY] = g_param_spec_enum (
      "category", "category", "The type of the module",
      GDN_MODULE_CATEGORY_TYPE, GDN_MODULE_CATEGORY_UNKNOWN, G_PARAM_READABLE);
}

static void
gdn_module_info_init (G_GNUC_UNUSED GdnModuleInfo *self)
{
}

static GdnModuleInfo *
module_info_new (const char *      name,
                 const char *      path,
                 const char *      abs_path,
                 GdnModuleCategory category)
{
  GdnModuleInfo *self = g_object_new (GDN_MODULE_INFO_TYPE, NULL);

  self->name = g_strdup (name);
  self->short_path = g_strdup (path);
  self->abs_path = g_strdup (abs_path);
  self->category = category;

  return self;
}

/* We assuming that the scheme value is a list of 4 entries
 * - name, a Guile string
 * - short-path, a Guile string
 * - abs-path, a Guile string
 * - category, a symbol, one of 'unknown, 'library, 'site, 'global-site,
 * 'package-data or 'other
 */
static GdnModuleInfo *
module_info_new_from_scm (SCM lst)
{
  SCM               s_name;
  SCM               s_short_path;
  SCM               s_abs_path;
  SCM               s_category;
  char *            name;
  char *            short_path;
  char *            abs_path;
  GdnModuleCategory category;

  s_name = scm_list_ref (lst, scm_from_int (0));
  s_short_path = scm_list_ref (lst, scm_from_int (1));
  s_abs_path = scm_list_ref (lst, scm_from_int (2));
  s_category = scm_list_ref (lst, scm_from_int (3));

  name = scm_to_utf8_string (s_name);
  short_path = scm_to_utf8_string (s_short_path);
  abs_path = scm_to_utf8_string (s_abs_path);

  if (scm_is_true (scm_eqv_p (scm_from_utf8_symbol ("unknown"), s_category)))
    category = GDN_MODULE_CATEGORY_UNKNOWN;
  else if (scm_is_true (
               scm_eqv_p (scm_from_utf8_symbol ("library"), s_category)))
    category = GDN_MODULE_CATEGORY_LIBRARY;
  else if (scm_is_true (scm_eqv_p (scm_from_utf8_symbol ("site"), s_category)))
    category = GDN_MODULE_CATEGORY_SITE;
  else if (scm_is_true (
               scm_eqv_p (scm_from_utf8_symbol ("global-site"), s_category)))
    category = GDN_MODULE_CATEGORY_GLOBAL_SITE;
  else if (scm_is_true (
               scm_eqv_p (scm_from_utf8_symbol ("package-data"), s_category)))
    category = GDN_MODULE_CATEGORY_PACKAGE_DATA;
  else if (scm_is_true (scm_eqv_p (scm_from_utf8_symbol ("other"), s_category)))
    category = GDN_MODULE_CATEGORY_OTHER;
  else
    category = GDN_MODULE_CATEGORY_UNKNOWN;

  GdnModuleInfo *info;
  info = module_info_new (name, short_path, abs_path, category);
  free (name);
  free (short_path);
  free (abs_path);
  return info;
}

void
gdn_module_info_store_append (GListStore *store, SCM entry)
{
  GdnModuleInfo *info;

  info = module_info_new_from_scm (entry);
  g_list_store_append (store, info);
}
