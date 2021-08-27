/* guidance-binding-info.c
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

#include "guidance-binding-info.h"

struct _GdnBindingInfo
{
  GObject parent;

  char *   name;
  gboolean argument;
  char *   representation;
  char *   value;
  char *   extra;
};

G_DEFINE_TYPE (GdnBindingInfo, gdn_binding_info, G_TYPE_OBJECT)

static GListStore *_store = NULL;

enum
{
  PROP_0,
  PROP_NAME,
  PROP_ARGUMENT,
  PROP_REPRESENTATION,
  PROP_VALUE,
  PROP_EXTRA,
  N_PROPS
};

static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static void
gdn_binding_info_get_property (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec)
{
  GdnBindingInfo *self = GDN_BINDING_INFO (object);

  switch (property_id)
    {
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_ARGUMENT:
      g_value_set_boolean (value, self->argument);
      break;
    case PROP_REPRESENTATION:
      g_value_set_string (value, self->representation);
      break;
    case PROP_VALUE:
      g_value_set_string (value, self->value);
      break;
    case PROP_EXTRA:
      g_value_set_string (value, self->extra);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_binding_info_finalize (GObject *object)
{
  GdnBindingInfo *self = GDN_BINDING_INFO (object);
  g_free (self->name);
  self->name = NULL;
  self->argument = FALSE;
  g_free (self->representation);
  self->representation = NULL;
  g_free (self->value);
  self->value = NULL;
  g_free (self->extra);
  self->extra = NULL;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_binding_info_parent_class)->finalize (object);
}

static void
gdn_binding_info_class_init (GdnBindingInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_binding_info_finalize;
  gobj_class->get_property = gdn_binding_info_get_property;

  properties[PROP_NAME] = g_param_spec_string ("name", "name", "binding name",
                                               NULL, G_PARAM_READABLE);
  properties[PROP_ARGUMENT] = g_param_spec_boolean ("argument", "argument", "argument flag",
                                                    FALSE, G_PARAM_READABLE);
  properties[PROP_REPRESENTATION] = g_param_spec_string ("representation", "representation", "binding representation",
                                                         NULL, G_PARAM_READABLE);
  properties[PROP_VALUE] = g_param_spec_string ("value", "value", "binding value",
                                                NULL, G_PARAM_READABLE);
  properties[PROP_EXTRA] = g_param_spec_string ("value", "value", "extra info",
                                                NULL, G_PARAM_READABLE);

}

static void
gdn_binding_info_init (G_GNUC_UNUSED GdnBindingInfo *self)
{
}

static GdnBindingInfo *
info_new_from_scm (SCM info)
{
  GdnBindingInfo *self = g_object_new (GDN_BINDING_INFO_TYPE, NULL);

  self->name = scm_to_utf8_string (scm_c_vector_ref (info, 0));
  self->argument = scm_is_true (scm_c_vector_ref (info, 1));
  self->representation = scm_to_utf8_string (scm_c_vector_ref (info, 2));
  self->value = scm_to_utf8_string (scm_c_vector_ref (info, 3));
  self->extra = scm_to_utf8_string (scm_c_vector_ref (info, 4));

  return self;
}

void
gdn_binding_info_update_all (SCM all_bindings)
{
  SCM             entry;
  GdnBindingInfo *info;
  size_t          i;

  if (_store == NULL)
    _store = g_list_store_new (GDN_BINDING_INFO_TYPE);

  g_list_store_remove_all (_store);

  for (i = 0; i < scm_c_vector_length (all_bindings); i++)
    {
      entry = scm_c_vector_ref (all_bindings, i);
      info = info_new_from_scm (entry);
      g_list_store_append (_store, info);
    }
}

GListStore *
gdn_binding_info_get_list_store (void)
{
  if (_store == NULL)
    _store = g_list_store_new (GDN_BINDING_INFO_TYPE);
  return _store;
}

gboolean
gdn_binding_info_get_argument (GdnBindingInfo *self)
{
  return self->argument;
}

const char *
gdn_binding_info_get_name (GdnBindingInfo *self)
{
  return self->name;
}

const char *
gdn_binding_info_get_representation (GdnBindingInfo *self)
{
  return self->representation;
}

const char *
gdn_binding_info_get_value (GdnBindingInfo *self)
{
  return self->value;
}

const char *
gdn_binding_info_get_extra (GdnBindingInfo *self)
{
  return self->extra;
}
