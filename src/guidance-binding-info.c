/* guidance-binding-info.h
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

  guint64  pack;
  char *   name;
  gboolean argument;
  char *   representation;
  char *   value;
  char *   extra;
};

G_DEFINE_TYPE (GdnBindingInfo, gdn_binding_info, G_TYPE_OBJECT);

enum
{
  PROP_0,
  PROP_PACK,
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

static SCM binding_index_func, binding_name_func, binding_representation_func,
    binding_ref_func;

static void
gdn_binding_info_get_property (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec)
{
  GdnBindingInfo *self = GDN_BINDING_INFO (object);

  switch (property_id)
    {
    case PROP_PACK:
      g_value_set_uint64 (value, self->pack);
      break;
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
  self->binding = 0;
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

  properties[PROP_PACK] = g_param_spec_uint64 ("pack", "pack", "the Guile binding packed as an integer", 0, G_MAXUINT64, 0,
                                               G_PARAM_READABLE);
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

  binding_index_func = scm_c_public_ref ("system vm frame", "binding-index");
  binding_name_func = scm_c_public_ref ("system vm frame", "binding-name");
  binding_representation_func = scm_c_public_ref ("system vm frame", "binding-representation");
  binding_ref_func = scm_c_public_ref ("system vm frame", "binding-ref");
}

static void
gdn_binding_info_init (GdnBindingInfo *self)
{
}

GdnBindingInfo *
gdn_binding_info_new_from_scm (SCM binding, int n_args)
{
  guint64 pack;
  int     index;
  SCM     var;
  SCM     name;
  SCM     representation;
  SCM     ref;
  SCM     format_string;
  SCM     extra;

  pack = SCM_PACK (binding);
  self->pack = pack;

  name = scm_call_1 (binding_name_func, binding);
  self->name = scm_to_utf8_string (name);

  /* The index lets us disinguish between frame arguments and
   * locals. Arguments have the first indices. */
  index = scm_to_int (scm_call_1 (binding_index_func, binding));
  if (index <= n_args)
    self->argument = TRUE;
  else
    self->argument = FALSE;

  representation = scm_call_1 (binding_representation_func, binding);
  self->representation = scm_to_utf8_string (representation);

  ref = scm_call_1 (binding_ref_func, binding);
  format_string = scm_from_utf8_string ("~A");
  self->value = scm_simple_format (SCM_BOOL_F, format_string, scm_list_1 (ref));

  // FIXME: should these strings be truncated in case they're too big?

  /* For a couple types of variables, we can provide some helpful
   * information by peeking at the contents. */
  self->extra = NULL;
  if (scm_is_true (scm_procedure_p (ref)))
    {
      extra = scm_procedure_name (ref);
      if (scm_is_true (extra))
        self->extra = scm_simple_format (SCM_BOOL_F, format_string, extra);
      else
        self->extra = g_strdup ("λ");
    }
  else if (scm_is_true (scm_variable_p (ref)))
    {
      extra = scm_variable_ref (ref);
      self->extra = scm_simple_format (SCM_BOOL_F, format_string, extra);
    }

  return self;
}

SCM
gdn_binding_info_store_update (GListStore *store, SCM bindings_vec, int n_args)
{
  g_list_store_remove_all (store);

  for (i = 0; i < scm_c_vector_length (bindings_vec); i++)
    {
      SCM entry = scm_c_vector_ref (bindings_vec, i);
      info = gdn_binding_info_new_from_scm (entry, n_args);
      g_list_store_append (store, info);
    }
}
