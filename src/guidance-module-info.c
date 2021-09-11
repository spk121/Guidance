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
gdn_module_info_category_get_type (void)
{
  static gsize gdn_module_info_category_type = 0;

  if (g_once_init_enter (&gdn_module_info_category_type))
    {
      static const GEnumValue values[] = {
        { GDN_MODULE_INFO_CATEGORY_UNKNOWN, "GDN_MODULE_INFO_CATEGORY_UNKNOWN",
          "unknown" },
        { GDN_MODULE_INFO_CATEGORY_LIBRARY, "GDN_MODULE_INFO_CATEGORY_LIBRARY",
          "category" },
        { GDN_MODULE_INFO_CATEGORY_MODULE, "GDN_MODULE_INFO_CATEGORY_MODULE",
          "module" },
        { GDN_MODULE_INFO_CATEGORY_PROCEDURE,
          "GDN_MODULE_INFO_CATEGORY_PROCEDURE", "procedure" },
        { 0, NULL, NULL }
      };
      GType type;

      type = g_enum_register_static ("GdnModuleInfoCategoryType", values);

      g_once_init_leave (&gdn_module_info_category_type, type);
    }
  return gdn_module_info_category_type;
}

struct _GdnModuleInfo
{
  GObject parent;

  guint64 pack;
  char *  name;
  GSList *list;
};

G_DEFINE_TYPE (GdnModuleInfo, gdn_module_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_PACK,
  PROP_NAME,
  PROP_LIST,
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
    case PROP_PACK:
      g_value_set_uint64 (value, self->value);
      break;
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_LIST:
      g_value_set_pointer (value, self->list);
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
  self->pack = NULL;
  if (self->list)
    {
      g_slist_free_full (self->list, gdn_module_info_finalize);
      self->list = NULL;
    }

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_module_info_parent_class)->finalize (object);
}

static void
gdn_module_info_class_init (GdnModuleInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_module_info_finalize;
  gobj_class->get_property = gdn_module_info_get_property;

  properties[PROP_PACK] =
      g_param_spec_uint64 ("pack", "pack", "A packed SCM value", 0, G_MAXUINT64,
                           0, G_PARAM_READABLE);
  properties[PROP_NAME] = g_param_spec_string (
      "name", "name", "The name of a module entry", NULL, G_PARAM_READABLE);
  properties[PROP_LIST] = g_param_spec_pointer (
      "list", "list", "list of children", G_PARAM_READABLE);
}

static void
gdn_module_info_init (G_GNUC_UNUSED GdnModuleInfo *self)
{
}

////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////

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
module_info_new_from_scm (SCM module_info)
{
  SCM               s_name;
  SCM               s_short_path;
  SCM               s_abs_path;
  SCM               s_category;
  char *            name;
  char *            short_path;
  char *            abs_path;
  GdnModuleCategory category;

  s_name = scm_c_vector_ref (module_info, 0);
  s_short_path = scm_c_vector_ref (module_info, 1);
  s_abs_path = scm_c_vector_ref (module_info, 2);
  s_category = scm_c_vector_ref (module_info, 3);

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

////////////////////////////////////////////////////////////////

static GdnModuleInfo *
module_info_new (GdnModuleCategory category,
                 const char *      name,
                 const char *      path,
                 const char *      abs_path)
{
  GdnModuleInfo *self = g_object_new (GDN_MODULE_INFO_TYPE, NULL);

  self->category = category;
  if (name != NULL)
    self->name = g_strdup (name);
  if (path != NULL)
    self->short_path = g_strdup (path);
  if (abs_path != NULL)
    self->abs_path = g_strdup (abs_path);

  return self;
}

GtkTreeListModel *
gdn_module_info_get_tree_model (void)
{
  GdnModuleInfo *entry;
  if (_store == NULL)
    {
      _store = g_object_new (GDN_TYPE_MODULE_INFO);

      /* Populate the top-level categories. */
      entry =
          module_info_new (GDN_MODULE_CATEGORY_LIBRARY, "library", NULL, NULL);
      g_list_store_append (_store, entry);
      entry = module_info_new (GDN_MODULE_CATEGORY_SITE, "site", NULL, NULL);
      g_list_store_append (_store, entry);
      entry = module_info_new (GDN_MODULE_CATEGORY_GLOBAL_SITE, "global site",
                               NULL, NULL);
      g_list_store_append (_store, entry);
      entry =
          module_info_new (GDN_MODULE_CATEGORY_PACKAGE, "package", NULL, NULL);
      g_list_store_append (_store, entry);
      entry = module_info_new (GDN_MODULE_CATEGORY_OTHER, "other", NULL, NULL);
      g_list_store_append (_store, entry);
    }
  if (_model == NULL)
    _model = gtk_tree_list_model_new (_store, FALSE, FALSE, get_child_model,
                                      NULL, NULL);
  return _model;
}

// A GtkTreeListModelCreateModelFunc
// Creates the children model of an GdnModuleInfo
static GListModel *
get_child_model (GObject *item, G_GNUC_UNUSED gpointer user_data)
{
  GdnModuleInfo *info;
  GListModel *   entries;

  info = GDN_MODULE_INFO (item);

  if (info->pack == 0)
    {
      /* with no associated SCM, this must be a top-level */
      if (info->list)
        {
          modules = g_list_store_new (GDN_ENVIRONMENT_INFO_TYPE);
          g_slist_foreach (info->list, pack_modules, entries);
          return entries;
        }
      else
        return NULL;
    }
  else if (scm_is_module (SCM_UNPACK (info->pack)))
    {
      /* For modules, query all the top-level functions of the module */
      /* If there are not top-level modules, return NULL and quit.
      /* Pack the variables. Get the names. No children per entry */
      /* Construct a list model */
    }
  else if (!scm_is_module (SCM_UNPACK (info->pack)))
    {
      /* This must be a variables, so it has no children. */
    }
  return NULL;
}

/*
 * This callback is used in the gdn-module-defined-hook, which is
 * called on each new module by the module-defined-hook.
 */
static SCM
scm_module_defined_handler (SCM scat, SCM sname, SCM spath, SCM sabspath)
{
  SCM_ASSERT_TYPE (scm_is_string (scat), scat, SCM_ARG1,
                   "gdn-module-defined-handler", "string");
  SCM_ASSERT_TYPE (scm_is_string (sname), sname, SCM_ARG2,
                   "gdn-module-defined-handler", "string");
  SCM_ASSERT_TYPE (scm_is_string (spath), spath, SCM_ARG3,
                   "gdn-module-defined-handler", "string");
  SCM_ASSERT_TYPE (scm_is_string (scat), sabspath, SCM_ARG4,
                   "gdn-module-defined-handler", "string");

  char *         cat = scm_to_utf8_string (scat);
  gboolean       ret;
  guint          pos;
  GdnModuleInfo *top_entry;
  GdnModuleInfo *new_entry;

  /* Find or create a top-level category for this entry. */
  ret =
      g_list_store_find_with_equal_func (_store, cat, category_equality, &pos);
  if (ret == FALSE)
    {
      top_entry = module_info_new (cat, NULL, NULL);
      g_list_store_append (_store, entry);
    }
  else
    top_entry = g_list_model_get_item (_store, pos);

  /* We sort the 2nd level entries by the relative path. */
  new_entry = module_info_new (cat, path, abspath);

  g_slist_insert_sorted_with_data (_store->list, new_entry, entry_compare);

  return SCM_UNSPECIFIED;
}

/* This should only be called by the module defined hook, so hopefully
 * we can be confident that module is a <directory> type. */
static SCM
scm_module_defined_handler (SCM module)
{
  SCM     s_name_symbol_list = scm_module_name (module);
  SCM     s_same = scm_object_to_string (s_name_symbol_list, SCM_UNDEFINED);
  char *  name = scm_to_utf8_string (s_name);
  guint64 pack = SCM_PACK_POINTER (module);

  GSList *list = NULL;

  // Figure out the absolute path of this module
  SCM            s_rel_path = scm_module_filename (module);
  SCM            s_abs_path = scm_sys_search_load_path (s_rel_path);
  size_t         abs_path_len = scm_c_string_length (s_abs_path);
  char *         abs_path = scm_to_utf8_string (s_abs_path);
  GdnModuleInfo *new_entry = g_object_new (GDN_TYPE_MODULE_INFO);
  new_entry->name = name;
  new_entry->pack = pack;
  new_entry->list = NULL;

  // Categorize this module
  int    i = 1;
  SCM    prefix;
  size_t prefix_len;
  while (i <= 3)
    {
      if (i == 1)
        prefix = scm_sys_library_dir ();
      else if (i == 2)
        prefix = scm_sys_site_dir ();
      else if (i == 3)
        prefix = scm_package_data_dir ();
      prefix_len = scm_c_string_length (prefix);

      if (prefix_len < abs_path_len)
        {
          if (scm_is_true (scm_string_eq (prefix, s_abs_path, scm_from_int (0),
                                          scm_from_int (prefix_len),
                                          scm_from_int (0),
                                          scm_from_int (prefix_len))))
            break;
        }
      i++;
    }
  char *category;
  if (i == 1)
    category = "library";
  else if (i == 2)
    category = "site";
  else if (i == 3)
    category = "package data";
  else
    category = "other";

  // Check to see if there is a parent list item for this category

  guint    position;
  gboolean match;
  match = g_list_store_find_with_equal_func (_store, category, category_equal,
                                             &position);
  if (match)
    {
      gpointer       item = g_list_model_get_item (_store, position);
      GdnModuleInfo *category_info = GDN_MODULE_INFO (item);
      category_info->list = g_slist_insert_sorted (category_info->list,
                                                   new_entry, module_compare);
    }
  else
    {
      GdnModuleInfo *new_category_info = g_object_new (GDN_TYPE_MODULE_INFO);
      new_category_info->name = g_strdup (category);
      new_category_info->pack = 0;
      new_category_info->list = NULL;
      g_list_store_append (_store, new_category_info);
      new_category_info->list =
          g_slist_append (new_category_info->list, new_entry);
    }
  return SCM_UNSPECIFIED;
}

static SCM
scm_module_p (SCM x)
{
  return scm_call_1 (scm_variable_ref (module_p_var), x);
}

static int
scm_is_module (SCM x)
{
  return scm_is_true (scm_module_p (x));
}

static SCM
scm_module_name (SCM module)
{
  return scm_call_1 (scm_variable_ref (module_name_var), module);
}

static SCM
scm_module_filename (SCM module)
{
  return scm_call_1 (scm_variable_ref (module_filename_var), module);
}

void
gdn_module_info_guile_init (void)
{
  GdnModuleInfo *entry;

  gdn_module_info_get_tree_model ();

  module_p_var = scm_c_lookup ("module?");
  module_name_var = scm_c_lookup ("module-name");
  module_filename_var = scm_c_lookup ("module-filename");
  module_defined_hook_var = scm_c_lookup ("module-defined-hook");

  SCM proc = scm_c_define_gsubr ("gdn-module-defined-handler", 1, 0, 0,
                                 scm_module_defined_handler);
  scm_c_add_hook (scm_variable_ref (module_defined_hook_var), proc, SCM_BOOL_F);
}
