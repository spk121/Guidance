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

struct _GdnEnvironmentInfo
{
  GObject parent;

  char *category;
  char *key;
  char *val;
};

G_DEFINE_TYPE (GdnEnvironmentInfo, gdn_environment_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_CATEGORY,
  PROP_KEY,
  PROP_VAL,
  N_PROPS
};
static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static void
gdn_environment_info_get_property (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec)
{
  GdnEnvironmentInfo *self = GDN_ENVIRONMENT_INFO (object);

  switch (property_id)
    {
    case PROP_CATEGORY:
      g_value_set_string (value, self->category);
      break;
    case PROP_KEY:
      g_value_set_string (value, self->key);
      break;
    case PROP_VAL:
      g_value_set_string (value, self->val);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_environment_info_finalize (GObject *object)
{
  GdnEnvironmentInfo *self = GDN_ENVIRONMENT_INFO (object);
  g_free (self->category);
  self->category = NULL;
  g_free (self->key);
  self->key = NULL;
  g_free (self->val);
  self->val = NULL;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_environment_info_parent_class)->finalize (object);
}

static void
gdn_environment_info_class_init (GdnEnvironmentInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_environment_info_finalize;
  gobj_class->get_property = gdn_environment_info_get_property;

  properties[PROP_CATEGORY] = g_param_spec_string ("category", "category", "category",
                                                   NULL, G_PARAM_READABLE);
  properties[PROP_KEY] = g_param_spec_string ("key", "key", "A name of some environment variable",
                                              NULL, G_PARAM_READABLE);
  properties[PROP_VAL] = g_param_spec_string (
      "val", "val", "The string value of an environment variable", NULL, G_PARAM_READABLE);
}

static void
gdn_environment_info_init (G_GNUC_UNUSED GdnEnvironmentInfo *self)
{
}

GdnEnvironmentInfo *
gdn_environment_info_new_from_scm (SCM info)
{
  GdnEnvironmentInfo *self = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);

  self->category = scm_to_utf8_string (scm_list_ref (info, scm_from_int (0)));
  self->key = scm_to_utf8_string (scm_list_ref (info, scm_from_int (1)));
  self->val = scm_to_utf8_string (scm_list_ref (info, scm_from_int (2)));
  return self;
}

void
gdn_environment_info_store_update (GListStore *store, SCM all_info)
{
  g_list_store_remove_all (store);

  for (size_t i = 0; i < scm_c_vector_length (all_info); i++)
    {
      SCM                 entry = scm_c_vector_ref (all_info, i);
      GdnEnvironmentInfo *info = gdn_environment_info_new_from_scm (entry);
      g_list_store_append (store, info);
    }
}
