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

////////////////////////////////////////////////////////////////
// TYPES
////////////////////////////////////////////////////////////////

typedef enum
{
  LIBRARY_DIR,
  SITE_DIR,
  PACKAGE_DATA_DIR,
  OTHER_DIR,
} dir_type_t;

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

  guint64 pack; /* A packed SCM of a module or procedure */
  char *  name;
  GSList *list; /* For top-levels, a list of modules */
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

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
static void gdn_module_info_finalize (GObject *object);
static void gdn_module_info_get_property (GObject *    object,
                                          unsigned int property_id,
                                          GValue *     value,
                                          GParamSpec * pspec);

static int         compare_info_by_name (GdnModuleInfo *a, GdnModuleInfo *b);
static GListStore *convert_slist_to_list_store (GSList *list);
static GListStore *create_module_bindings_list_store (SCM module);
static void        find_top_level_info (const char *name, guint *pos);
static GListModel *get_child_model (GObject *              item,
                                    G_GNUC_UNUSED gpointer user_data);
static char *      get_module_abs_path (SCM module, size_t *len);
static dir_type_t  get_path_category (const char *path);

static int scm_is_module (SCM x);
static SCM scm_module_filename (SCM module);
static SCM scm_module_name (SCM module);
static SCM scm_module_obarray (SCM module);

static void module_info_add_module_to_store (gpointer key,
                                             gpointer value,
                                             gpointer user_data);
static int  module_info_compare (const void *a, const void *b, void *user_data);

GHashTable *      _scm_modules = NULL;
GListStore *      _store = NULL;
GtkTreeListModel *_model = NULL;

SCM make_info_from_binding_proc;
SCM module_p_var;
SCM module_name_var;
SCM module_filename_var;
SCM module_obarray_var;
SCM module_defined_hook_var;

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

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

static void
gdn_module_info_finalize (GObject *object)
{
  GdnModuleInfo *self = GDN_MODULE_INFO (object);
  free (self->name);
  self->name = NULL;
  self->pack = 0;
  if (self->list)
    {
      g_slist_free_full (self->list, (GDestroyNotify) gdn_module_info_finalize);
      self->list = NULL;
    }

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_module_info_parent_class)->finalize (object);
}

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
      g_value_set_uint64 (value, self->pack);
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

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

GtkTreeListModel *
gdn_module_info_get_tree_model (void)
{
  if (_store == NULL)
    _store = g_list_store_new (GDN_MODULE_INFO_TYPE);
  if (_model == NULL)
    _model = gtk_tree_list_model_new (
        G_LIST_MODEL (_store), FALSE, FALSE,
        (GtkTreeListModelCreateModelFunc) get_child_model, NULL, NULL);
  return _model;
}

gboolean
gdn_module_info_is_top_level (GdnModuleInfo *info)
{
  if (info->pack == 0)
    return TRUE;
  return FALSE;
}

gboolean
gdn_module_info_is_module (GdnModuleInfo *info)
{
  if (info->pack && scm_is_module (SCM_PACK (info->pack)))
    return TRUE;
  return FALSE;
}

gboolean
gdn_module_info_is_procedure (GdnModuleInfo *info)
{
  if (info->pack && !scm_is_module (SCM_PACK (info->pack)))
    return TRUE;
  return FALSE;
}

const char *
gdn_module_info_get_name (GdnModuleInfo *info)
{
  return (info->name);
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static GListStore *
convert_slist_to_list_store (GSList *list)
{
  GSList *    cur;
  GListStore *entries;

  if (list == NULL)
    return NULL;
  entries = g_list_store_new (GDN_MODULE_INFO_TYPE);

  cur = list;
  while (cur != NULL)
    {
      g_list_store_append (entries, cur->data);
      cur = cur->next;
    }
  return entries;
}

// A GtkTreeListModelCreateModelFunc
// Creates the children model of an GdnModuleInfo
static GListModel *
get_child_model (GObject *item, G_GNUC_UNUSED gpointer user_data)
{
  GdnModuleInfo *info;

  info = GDN_MODULE_INFO (item);

  if (info->list)
    {
      /* Only top-level categories should have lists */
      g_assert (info->pack == 0);

      return G_LIST_MODEL (convert_slist_to_list_store (info->list));
    }

  if (info->pack && scm_is_module (SCM_PACK (info->pack)))
    return G_LIST_MODEL (
        create_module_bindings_list_store (SCM_PACK (info->pack)));

  return NULL;
}

/* FIXME: hacky */
GListStore *_inner_store;

static SCM
make_info_from_binding (SCM key, SCM value)
{
  GdnModuleInfo *info;
  info = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  info->pack = SCM_UNPACK (value);
  info->name = scm_to_utf8_string (scm_symbol_to_string (key));
  info->list = NULL;
  g_list_store_append (_inner_store, info);

  return SCM_UNSPECIFIED;
}

static GListStore *
create_module_bindings_list_store (SCM module)
{
  SCM hash;
  _inner_store = g_list_store_new (GDN_MODULE_INFO_TYPE);
  hash = scm_module_obarray (module);
  scm_hash_for_each (make_info_from_binding_proc, hash);
  return _inner_store;
}

static gboolean
match_info_name (GdnModuleInfo *a, GdnModuleInfo *b)
{
  if (strcmp (a->name, b->name) == 0)
    return TRUE;
  return FALSE;
}

static int
compare_info_by_name (GdnModuleInfo *a, GdnModuleInfo *b)
{
  return strcmp (a->name, b->name);
}

static void
find_top_level_info (const char *name, guint *pos)
{
  gboolean       match;
  GdnModuleInfo *info = NULL;

  g_assert (pos != NULL);

  info = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  info->name = g_strdup (name);
  info->pack = 0;
  info->list = NULL;

  match = g_list_store_find_with_equal_func (_store, (void *) info,
                                             (GEqualFunc) match_info_name, pos);
  if (match)
    return;

  g_list_store_append (_store, info);

  find_top_level_info (name, pos);
}

/* Given a module, return a string containing the absolute filepath of
 * the module, or NULL.  LEN, if provided will contain the octet
 * length of the absolute path string. */
static char *
get_module_abs_path (SCM module, size_t *len)
{
  SCM rel_path, abs_path;

  rel_path = scm_module_filename (module);
  if (scm_is_false (rel_path))
    return NULL;
  abs_path = scm_sys_search_load_path (rel_path);
  if (scm_is_false (abs_path))
    return NULL;

  if (len)
    *len = scm_c_string_length (abs_path);
  return scm_to_utf8_string (abs_path);
}

static dir_type_t
get_path_category (const char *path)
{
  char * prefix;
  size_t prefix_len;

  // Categorize this module
  for (int i = 0; i < 3; i++)
    {
      int cmp;

      if (i == LIBRARY_DIR)
        prefix = scm_to_utf8_string (scm_sys_library_dir ());
      else if (i == SITE_DIR)
        prefix = scm_to_utf8_string (scm_sys_site_dir ());
      else if (i == PACKAGE_DATA_DIR)
        prefix = scm_to_utf8_string (scm_sys_package_data_dir ());
      else
        abort ();
      prefix_len = strlen (prefix);

      cmp = strncmp (prefix, path, prefix_len);
      free (prefix);
      if (cmp == 0)
        return i;
    }
  return OTHER_DIR;
}

void
gdn_module_info_store_update (void)
{
  if (_scm_modules == NULL)
    return;

  g_list_store_remove_all (_store);
  g_hash_table_foreach (_scm_modules, module_info_add_module_to_store, _store);
}

static void
module_info_add_module_to_store (gpointer key,
                                 gpointer value,
                                 gpointer user_data)
{
  GListStore *   store = G_LIST_STORE (user_data);
  const char *   name = key;
  GdnModuleInfo *entry = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  entry->name = g_strdup (name);
  entry->pack = GPOINTER_TO_SIZE (value);
  g_list_store_insert_sorted (store, entry, module_info_compare, NULL);
}

static int
module_info_compare (const void *a, const void *b, void *user_data)
{
  g_assert (user_data == NULL);
  const GdnModuleInfo *entry_a = a;
  const GdnModuleInfo *entry_b = b;
  return strcmp (entry_a->name, entry_b->name);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

static SCM
scm_add_module (SCM module)
{
  SCM      s_name_symbol_list, s_name;
  char *   name;
  gboolean ret;

  SCM_ASSERT (scm_is_module (module), module, SCM_ARG1, "gdn-add-module");
  s_name_symbol_list = scm_module_name (module);
  s_name = scm_object_to_string (s_name_symbol_list, SCM_UNDEFINED);
  name = scm_to_utf8_string (s_name);

  ret = g_hash_table_insert (_scm_modules, name, module);
  if (ret)
    g_debug ("added new module %s", name);
  gdn_module_info_store_update ();
  return SCM_UNSPECIFIED;
}

#if 0
static SCM
scm_add_module (SCM module)
{
  SCM            s_name_symbol_list, s_name;
  char *         name, *abs_path;
  size_t         abs_path_len;
  GdnModuleInfo *new_entry, *category_info;
  dir_type_t     dir_type;

  const char *dir_names[4] = { "library", "site", "package data",
                               "application" };

  SCM_ASSERT (scm_is_module (module), module, SCM_ARG1, "gdn-add-module");
  s_name_symbol_list = scm_module_name (module);
  s_name = scm_object_to_string (s_name_symbol_list, SCM_UNDEFINED);
  name = scm_to_utf8_string (s_name);

  // Figure out the absolute path of this module
  abs_path = get_module_abs_path (module, &abs_path_len);
  dir_type = get_path_category (abs_path);
  free (abs_path);
  abs_path = NULL;

  new_entry = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  new_entry->name = name;
  new_entry->pack = SCM_UNPACK (module);
  new_entry->list = NULL;

  const char *category = dir_names[dir_type];

  // Check to see if there is a parent list item for this category
  guint position;

  find_top_level_info (category, &position);
  category_info =
      GDN_MODULE_INFO (g_list_model_get_item (G_LIST_MODEL (_store), position));
  category_info->list = g_slist_insert_sorted (
      category_info->list, new_entry, (GCompareFunc) compare_info_by_name);
  g_list_model_items_changed (_store, position, 0, 0);
  return SCM_UNSPECIFIED;
}
#endif

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

static SCM
scm_module_obarray (SCM module)
{
  return scm_call_1 (scm_variable_ref (module_obarray_var), module);
}

void
gdn_module_info_guile_init (void)
{
  _scm_modules = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
  gdn_module_info_get_tree_model ();

  module_p_var = scm_c_lookup ("module?");
  module_name_var = scm_c_lookup ("module-name");
  module_filename_var = scm_c_lookup ("module-filename");
  module_defined_hook_var = scm_c_lookup ("module-defined-hook");
  module_obarray_var = scm_c_lookup ("module-obarray");

  make_info_from_binding_proc =
      scm_c_define_gsubr ("gdn-make-info-from-binding", 2, 0, 0,
                          (scm_t_subr) make_info_from_binding);

  SCM proc = scm_c_define_gsubr ("gdn-add-module", 1, 0, 0,
                                 (scm_t_subr) scm_add_module);
  scm_add_hook_x (scm_variable_ref (module_defined_hook_var), proc, SCM_BOOL_F);
}

////////////////////////////////////////////////////////////////
// END

#if 0
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

#endif
