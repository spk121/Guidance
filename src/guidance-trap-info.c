/* guidance-trap-info.c
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

#include "guidance-trap-info.h"
#include "guidance-lisp.h"

struct _GdnTrapInfo
{
  GObject parent;

  int      index;
  char *   name;
  gboolean enabled;
  gboolean current;
};

G_DEFINE_TYPE (GdnTrapInfo, gdn_trap_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_INDEX,
  PROP_NAME,
  PROP_ENABLED,
  PROP_CURRENT,
  N_PROPS
};

static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static SCM trap_name_func, trap_enabled_func;

static void
gdn_trap_info_get_property (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec)
{
  GdnTrapInfo *self = GDN_TRAP_INFO (object);

  switch (property_id)
    {
    case PROP_INDEX:
      g_value_set_int (value, self->index);
      break;
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_ENABLED:
      g_value_set_boolean (value, self->enabled);
      break;
    case PROP_CURRENT:
      g_value_set_boolean (value, self->current);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_trap_info_set_property (GObject *object, unsigned int property_id, const GValue *value, GParamSpec *pspec)
{
  GdnTrapInfo *self = GDN_TRAP_INFO (object);

  switch (property_id)
    {
      // case PROP_NAME:
      // case PROP_INDEX:
    case PROP_ENABLED:
      self->enabled = g_value_get_boolean (value);
      break;
    case PROP_CURRENT:
      self->current = g_value_get_boolean (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_trap_info_finalize (GObject *object)
{
  GdnTrapInfo *self = GDN_TRAP_INFO (object);
  self->index = 0;
  free (self->name);
  self->name = NULL;
  self->enabled = FALSE;
  self->current = FALSE;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_trap_info_parent_class)->finalize (object);
}

static void
gdn_trap_info_class_init (GdnTrapInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_trap_info_finalize;
  gobj_class->get_property = gdn_trap_info_get_property;
  gobj_class->set_property = gdn_trap_info_set_property;

  properties[PROP_INDEX] =
      g_param_spec_int ("index", "index", "trap index", 0, G_MAXINT, 0, G_PARAM_READABLE);
  properties[PROP_NAME] =
      g_param_spec_string ("name", "name", "trap name", NULL, G_PARAM_READABLE);
  properties[PROP_ENABLED] =
      g_param_spec_boolean ("enabled", "enable", "enabled flag", TRUE, G_PARAM_READWRITE);
  properties[PROP_CURRENT] =
      g_param_spec_boolean ("current", "current", "current flag", FALSE, G_PARAM_READWRITE);

}

static void
gdn_trap_info_init (G_GNUC_UNUSED GdnTrapInfo *self)
{
}

GdnTrapInfo *
trap_info_new_from_trap_id (int trap, int current)
{
  GdnTrapInfo *self = g_object_new (GDN_TRAP_INFO_TYPE, NULL);
  SCM          s_id = scm_from_int (trap);

  self->index = trap;
  self->name = scm_to_utf8_string (scm_call_1 (trap_name_func, s_id));
  self->enabled = scm_is_true (scm_call_1 (trap_enabled_func, s_id));
  self->current = current;

  return self;
}

void
gdn_trap_info_guile_init (void)
{
  // list_traps_func = scm_c_public_ref ("system vm trap-state", "list-traps");
  trap_name_func = scm_c_public_ref ("system vm trap-state", "trap-name");
  trap_enabled_func =
      scm_c_public_ref ("system vm trap-state", "trap-enabled?");
}

int
gdn_trap_info_get_index (GdnTrapInfo *info)
{
  return info->index;
}

const char *
gdn_trap_info_get_name (GdnTrapInfo *info)
{
  return info->name;
}

gboolean
gdn_trap_info_get_active (GdnTrapInfo *info)
{
  return info->enabled;
}
