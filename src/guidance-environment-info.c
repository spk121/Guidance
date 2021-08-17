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

#include "guidance-environment-info.h"

////////////////////////////////////////////////////////////////
// GDN_ENVIRONMENT_CATEGORY

struct _GdnEnvironmentInfo
{
  GObject parent;

  char *key;
  char *      value;
  GHashTable *hash_table;
};

G_DEFINE_TYPE (GdnEnvironmentInfo, gdn_environment_info, G_TYPE_OBJECT)

GtkTreeListModel *_model = NULL;
GListStore *      _store = NULL;

static void
gdn_environment_info_finalize (GObject *object)
{
  GdnEnvironmentInfo *self = GDN_ENVIRONMENT_INFO (object);
  g_free (self->key);
  self->key = NULL;
  if (self->value)
    {
      g_free (self->value);
      self->value = NULL;
    }
  if (self->hash_table)
    {
      g_hash_table_destroy (self->hash_table);
      self->hash_table = NULL;
    }

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_environment_info_parent_class)->finalize (object);
}

static void
gdn_environment_info_class_init (GdnEnvironmentInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_environment_info_finalize;
}

static void
gdn_environment_info_init (G_GNUC_UNUSED GdnEnvironmentInfo *self)
{
}

////////////////////////////////////////////////////////////////
// POPULATE THE G_LIST_MODEL

static GdnEnvironmentInfo *
info_new_from_scm (SCM info)
{
  GdnEnvironmentInfo *self = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
  SCM                 entries;
  size_t              i;

  /* The top-level category keys don't have values. They're just
     category names.
   */
  self->key = scm_to_utf8_string (scm_c_vector_ref (info, 0));
  self->value = NULL;
  self->hash_table =
      g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

  entries = scm_c_vector_ref (info, 1);
  g_assert (scm_c_vector_length (entries) > 0);

  // Note that we only handle one level of children, not an arbitrary
  // level of nesting.

  for (i = 0; i < scm_c_vector_length (entries); i++)
    {
      SCM   child = scm_c_vector_ref (entries, i);
      char *key;
      char *val;
      key = scm_to_utf8_string (scm_c_vector_ref (child, 0));
      val = scm_to_utf8_string (scm_c_vector_ref (child, 1));
      g_hash_table_insert (self->hash_table, key, val);
    }
  return self;
}

// A GHFunc
// Pushes each entry of a GdnEnvironmentCategories's entry hash
// into a list store of type GDN_ENVIRONMENT_ENTRY_TYPE
static void
pack_entries (gpointer key, gpointer value, gpointer user_data)
{
  GListStore *        store = G_LIST_STORE (user_data);
  GdnEnvironmentInfo *entry = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
  entry->key = g_strdup (key);
  entry->value = g_strdup (value);
  entry->hash_table = NULL;
  g_list_store_append (store, entry);
}

// A GtkTreeListModelCreateModelFunc
// Creates the 1st level children of an GdnEnvironmentCategory
static GListModel *
get_child_model (GObject *item, G_GNUC_UNUSED gpointer user_data)
{
  GdnEnvironmentInfo *info;
  GListModel *        entries;

  info = GDN_ENVIRONMENT_INFO (item);
  if (info->hash_table)
    {
      entries = g_list_store_new (GDN_ENVIRONMENT_INFO_TYPE);
      g_hash_table_foreach (info->hash_table, pack_entries, entries);
      return entries;
    }

  return NULL;
}

void
gdn_environment_info_update (SCM all_info)
{
  SCM                 entry;
  GdnEnvironmentInfo *info;
  size_t              i;

  g_list_store_remove_all (_store);

  for (i = 0; i < scm_c_vector_length (all_info); i++)
    {
      entry = scm_c_vector_ref (all_info, i);
      info = info_new_from_scm (entry);
      g_list_store_append (_store, info);
    }
}

GtkTreeListModel *
gdn_environment_info_get_tree_model (void)
{
  if (_store == NULL)
    _store = g_list_store_new (GDN_ENVIRONMENT_INFO_TYPE);
  if (_model == NULL)
    _model = gtk_tree_list_model_new (_store, FALSE, FALSE, get_child_model,
                                      NULL, NULL);
  return _model;
}

const char *
gdn_environment_info_get_key (GdnEnvironmentInfo *info)
{
  return info->key;
}
const char *
gdn_environment_info_get_value (GdnEnvironmentInfo *info)
{
  return info->value;
}
