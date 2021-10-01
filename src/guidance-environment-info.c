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

G_DEFINE_TYPE (GdnEnvironmentInfo, gdn_environment_info, G_TYPE_OBJECT)

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static GtkTreeListModel *_model = NULL;
static GListStore *      _store = NULL;

static void                finalize (GdnEnvironmentInfo *self);
static GdnEnvironmentInfo *info_new_from_scm (SCM info);
static GListModel *        get_child_model (GObject *item, gpointer user_data);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_environment_info_class_init (GdnEnvironmentInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = finalize;
}

static void
gdn_environment_info_init (G_GNUC_UNUSED GdnEnvironmentInfo *self)
{
}

////////////////////////////////////////////////////////////////
// METHODS
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

static void
finalize (GdnEnvironmentInfo *self)
{
  g_free (self->key);
  self->key = NULL;
  if (self->value)
    {
      g_free (self->value);
      self->value = NULL;
    }
  if (self->list)
    {
      g_slist_free_full (self->list, (GDestroyNotify) finalize);
      self->list = NULL;
    }

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_environment_info_parent_class)->finalize (self);
}

GdnEnvironmentInfo *
gdn_environment_info_new_from_scm (SCM info)
{
  GdnEnvironmentInfo *self = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
  SCM                 entries;
  size_t              i;

  /* The top-level category keys don't have values. They're just
     category names.
   */
  self->key = scm_to_utf8_string (scm_c_vector_ref (info, 0));
  self->value = NULL;
  self->list = NULL;

  entries = scm_c_vector_ref (info, 1);
  g_assert (scm_c_vector_length (entries) > 0);

  // Note that we only handle one level of children, not an arbitrary
  // level of nesting.

  for (i = 0; i < scm_c_vector_length (entries); i++)
    {
      SCM   child = scm_c_vector_ref (entries, i);
      GdnEnvironmentInfo *kiddo =
          g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);

      kiddo->key = scm_to_utf8_string (scm_c_vector_ref (child, 0));
      kiddo->value = scm_to_utf8_string (scm_c_vector_ref (child, 1));
      kiddo->list = NULL;
      self->list = g_slist_append (self->list, kiddo);
    }
  return self;
}

GListStore *
gdn_environment_info_get_child_model (GdnEnvironmentInfo *info)
{
  GListStore *        entries;
  GSList *            lst;
  if (info->list)
    {
      entries = g_list_store_new (GDN_ENVIRONMENT_INFO_TYPE);
      lst = info->list;
      while (1)
        {
          g_list_store_append (entries, lst->data);
          if (lst->next == NULL)
            break;
          lst = lst->next;
        }
      return entries;
    }

  return NULL;
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////
#if 0
SCM
gdn_environment_info_update_all (SCM SCM all_info)
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

static gboolean
info_key_eq (gconstpointer a, gconstpointer b)
{
  const GdnEnvironmentInfo *ia = GDN_ENVIRONMENT_INFO (a);
  const GdnEnvironmentInfo *ib = GDN_ENVIRONMENT_INFO (b);
  return (g_strcmp0 (ia->key, ib->key) == 0);
}

void
gdn_environment_info_update_one (const char *category,
                                 const char *key,
                                 const char *value)
{
  GdnEnvironmentInfo *category_info;
  GdnEnvironmentInfo *entry_info;
  GdnEnvironmentInfo *temp_info =
      g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
  gboolean found;
  guint    position;

  temp_info->key = g_strdup (category);

  /* First check to see if there exists a top-level category by this
   * name, otherwise create one. */
  found = g_list_store_find_with_equal_func (_store, temp_info, info_key_eq,
                                             &position);
  if (!found)
    {
      category_info = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
      category_info->key = g_strdup (category);
      category_info->value = NULL;
      category_info->list = NULL;
      g_list_store_append (_store, category_info);
    }
  else
    category_info = g_list_model_get_item (_store, position);

  /* Now check to see if there is a child. Either update an existing child
   * or create a new one. */
  temp_info->key = key;
  GSList *entry =
      g_slist_find_custom (category_info->list, temp_info, info_key_eq);
  if (entry)
    {
      /* Overwrite an existing entry value */
      entry_info = entry->data;
      entry_info->value = g_strdup (value);
    }
  else
    {
      /* Insert a new one. Note that we don't worry about the ordering
       * here, because the update_all function re-writes everything
       * from scratch.  The only way we'd get here is right at the
       * beginning, before a program is started. */
      temp_info->key = g_strdup (key);
      temp_info->value = g_strdup (value);
      temp_info->list = NULL;
      category_info->list = g_slist_append (category_info->list, temp_info);
    }
}
#endif

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

char *
memory_string (size_t x)
{
  if (x < 1000)
    return g_strdup_printf ("%d B", x);
  else if (x < 1000000)
    return g_strdup_printf ("%.3f kB", x / 1000.0);
  else if (x < 1000000000)
    return g_strdup_printf ("%.3f MB", x / 1000000.0);
  else if (x < 1000000000000ull)
    return g_strdup_printf ("%.3f GB", x / 1000000000.0);

  return g_strdup_printf ("%.3f TB", x / 1000000000000.0);
}

char *
time_string (double x)
{
  if (x < 1.0e-6)
    return g_strdup_printf ("%.3f ns", x * 1.0e9);
  else if (x < 1.0e-3)
    return g_strdup_printf ("%.3f µs", x * 1.0e6);
  else if (x < 0.0)
    return g_strdup_printf ("%.3f ms", x * 1.0e3);
  else if (x < 60.0)
    return g_strdup_printf ("%.3f s", x);
  else if (x < 3600.0)
    return g_strdup_printf ("%.3f min", x / 60.0);
  else if (x < 24.0 * 3600.0)
    return g_strdup_printf ("%.3f h", x / 3600.0);
  else if (x < 365.0 * 24.0 * 3600.0)
    return g_strdup_printf ("%.3f d", x / (24.0 * 3600.0));

  return g_strdup_printf ("%.3f yr", x / (365.0 * 24.0 * 60.0 * 60.0));
}
