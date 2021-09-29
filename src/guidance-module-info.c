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

struct _GdnModuleInfo
{
  GObject parent;

  uint64_t pack; /* A packed SCM of a module or procedure */
  char *  name;
};

G_DEFINE_TYPE (GdnModuleInfo, gdn_module_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_PACK,
  PROP_NAME,
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

static GListStore *create_module_bindings_list_store (SCM module);
static GListModel *get_child_model (GObject *item, void *user_data);
static char *      get_module_abs_path (SCM module, size_t *len);

static uint64_t scm_unpack_to_uint64 (SCM x);
static SCM      uint64_pack_to_scm (uint64_t x);
static int scm_is_module (SCM x);
static SCM scm_module_filename (SCM module);
static SCM scm_module_name (SCM module);
static SCM scm_module_obarray (SCM module);

static int module_info_compare (const void *a, const void *b, void *user_data);
static int module_info_eq (const void *a, const void *b);

/* This scratch workspace is used as internal storage when creating
 * 2nd-level GListStore of module's procedure. */
static GListStore *_inner_store = NULL;

/* Holds the top-level list store of modules. */
static GListStore *_store = NULL;

/* Expresses _store as a tree list */
static GtkTreeListModel *_model = NULL;

static SCM add_binding_key_value_proc;
static SCM module_p_proc;
static SCM module_name_proc;
static SCM module_filename_proc;
static SCM module_obarray_proc;

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
gdn_module_info_is_module (GdnModuleInfo *info)
{
  if (info->pack && scm_is_module (uint64_pack_to_scm (info->pack)))
    return TRUE;
  return FALSE;
}

gboolean
gdn_module_info_is_procedure (GdnModuleInfo *info)
{
  if (info->pack && !scm_is_module (uint64_pack_to_scm (info->pack)))
    return TRUE;
  return FALSE;
}

const char *
gdn_module_info_get_name (GdnModuleInfo *info)
{
  return info->name;
}

char *
gdn_module_info_get_abs_path (GdnModuleInfo *info)
{
  g_assert (info != NULL);
  SCM module = uint64_pack_to_scm (info->pack);
  g_assert (scm_is_module (module));
  return get_module_abs_path (module, NULL);
}

SCM
gdn_module_info_get_procedure (GdnModuleInfo *info)
{
  return uint64_pack_to_scm (info->pack);
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static uint64_t
scm_unpack_to_uint64 (SCM x)
{
  scm_t_bits x_bits = SCM_UNPACK (x);
  g_assert (x_bits != 0);
  return (uint64_t) x_bits;
}

static SCM
uint64_pack_to_scm (uint64_t x)
{
  g_assert (x != 0);
  scm_t_bits x_bits = (scm_t_bits) x;
  return SCM_PACK (x_bits);
}

// A GtkTreeListModelCreateModelFunc
// Creates the children model of an GdnModuleInfo
static GListModel *
get_child_model (GObject *item, void *user_data)
{
  g_assert (item != NULL);
  g_assert (user_data == NULL);

  GdnModuleInfo *info;
  SCM            module;

  info = GDN_MODULE_INFO (item);

  if (info->pack)
    {
      module = uint64_pack_to_scm (info->pack);
      if (scm_is_module (module))
        return create_module_bindings_list_store (module);
    }

  return NULL;
}

/* This procedure takes an SCM module and creates a new GListStore
 * that contains an entry for each top-level procedure in the
 * module. */
static GListStore *
create_module_bindings_list_store (SCM module)
{
  SCM hash;
  _inner_store = g_list_store_new (GDN_MODULE_INFO_TYPE);
  hash = scm_module_obarray (module);
  scm_hash_for_each (add_binding_key_value_proc, hash);
  return _inner_store;
}

/* Given a module, this returns a string containing the absolute
 * filepath of the module, or NULL.  LEN, if provided will contain the
 * octet length of the absolute path string. */
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

/* A GCompareDataFunc that comparestwo GdnModuleInfo by their name. */
static int
module_info_compare (const void *a, const void *b, void *user_data)
{
  g_assert (user_data == NULL);
  const GdnModuleInfo *entry_a = a;
  const GdnModuleInfo *entry_b = b;
  return strcmp (entry_a->name, entry_b->name);
}

/* A GEqualFunc that returns TRUE when two GdnModuleInfo have the same
 * name. */
static int
module_info_eq (const void *a, const void *b)
{
  const GdnModuleInfo *entry_a = a;
  const GdnModuleInfo *entry_b = b;
  return strcmp (entry_a->name, entry_b->name) == 0;
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

/* This procedure is an internal procedure used when creating a
 * GListStore of top-level procedures from an SCM module. This
 * unfortunate proc is a work-around because of having to use
 * scm_hash_for_each. */
static SCM
scm_add_binding_key_value (SCM key, SCM value)
{
  GdnModuleInfo *info;
  info = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  info->pack = scm_unpack_to_uint64 (value);
  info->name = scm_to_utf8_string (scm_symbol_to_string (key));
  g_list_store_insert_sorted (_inner_store, info, module_info_compare, NULL);

  return SCM_UNSPECIFIED;
}

static SCM
scm_module_p (SCM x)
{
  return scm_call_1 (module_p_proc, x);
}

static int
scm_is_module (SCM x)
{
  return scm_is_true (scm_module_p (x));
}

static SCM
scm_module_name (SCM module)
{
  return scm_call_1 (module_name_proc, module);
}

static SCM
scm_module_filename (SCM module)
{
  return scm_call_1 (module_filename_proc, module);
}

static SCM
scm_module_obarray (SCM module)
{
  return scm_call_1 (module_obarray_proc, module);
}

/* This Guile procedure, given a Guile #<directory> module entry,
 * updates the module info _STORE. If an existing entry by this name
 * exists, it is replaced. Returns #t if _STORE is modified. */
static SCM
scm_add_module (SCM module)
{
  g_assert (_store != NULL);
  g_assert (_model != NULL);
  g_assert (scm_is_module (module));

  SCM            symlist;
  SCM            name;
  char *         str;
  GdnModuleInfo *entry, *found;
  guint          position;
  SCM            ret = SCM_BOOL_F;

  symlist = scm_module_name (module);
  name = scm_object_to_string (symlist, SCM_UNDEFINED);
  str = scm_to_utf8_string (name);
  entry = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  entry->name = str;
  entry->pack = scm_unpack_to_uint64 (module);

  if (g_list_store_find_with_equal_func (_store, entry, module_info_eq,
                                         &position))
    {
      found = g_list_model_get_item (_store, position);
      if (found->pack != entry->pack)
        {
          found->pack = entry->pack;
          g_list_model_items_changed (_store, position, 1, 1);
          ret = SCM_BOOL_T;
        }
      g_object_unref (entry);
    }
  else
    {
      g_list_store_insert_sorted (_store, entry, module_info_compare, NULL);
      ret = SCM_BOOL_T;
    }

  return ret;
}

/* This Guile procedure deletes all the entries in the module info
 * _STORE.  The return value is unspecified. */
static SCM
scm_clear_modules (void)
{
  g_assert (_store != NULL);

  g_list_store_remove_all (_store);
  return SCM_UNSPECIFIED;
}

/* This procedure initializes the Guile API for the Module page of the
   GUI. */
void
gdn_module_info_guile_init (void)
{
  /* Pre-initialize the local storage of module info. */
  gdn_module_info_get_tree_model ();

  /* Guile functions used in this module. */
  add_binding_key_value_proc =
      scm_c_define_gsubr ("%gdn-add-binding-key-value", 2, 0, 0,
                          (scm_t_subr) scm_add_binding_key_value);
  module_filename_proc = scm_variable_ref (scm_c_lookup ("module-filename"));
  module_name_proc = scm_variable_ref (scm_c_lookup ("module-name"));
  module_obarray_proc = scm_variable_ref (scm_c_lookup ("module-obarray"));
  module_p_proc = scm_variable_ref (scm_c_lookup ("module?"));

  /* The public API */
  scm_c_define_gsubr ("gdn-add-module", 1, 0, 0, (scm_t_subr) scm_add_module);
  scm_c_define_gsubr ("gdn-clear-modules", 0, 0, 0,
                      (scm_t_subr) scm_clear_modules);
}

