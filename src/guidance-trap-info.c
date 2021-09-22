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

static SCM list_traps_func, trap_name_func, trap_enabled_func;
static SCM trap_thunk_store[9], trap_func_store[9];
static int trap_thunk_index = 1;

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

static GdnTrapInfo *
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

/* GUILE THREAD: This updates the trap info store with information
 * about the current traps. Since the list of traps is thread
 * specific, this procedure must be called from the current Guile
 * thread. */
SCM
scm_update_traps (void)
{
  SCM all_traps = scm_vector (scm_call_0 (list_traps_func));

  g_list_store_remove_all (store);

  for (size_t i = 0; i < scm_c_vector_length (all_traps); i++)
    {
      int          trap;
      GdnTrapInfo *info;
      int          current;

      trap = scm_to_int (scm_c_vector_ref (all_traps, i));
      if (trap == trap_cur)
        current = TRUE;
      else
        current = FALSE;
      info = trap_info_new_from_trap_id (trap, current);
      g_list_store_append (store, info);
    }
}

/* GTK THREAD: This sends sends an async request to Guile to add a
 * trap at a given Guile procedure, at the next async opportunity. */
int
gdn_lisp_add_proc_trap_async (SCM proc)
{
  SCM default_thread = gdn_lisp_get_default_thread ();

  trap_func_store[trap_thunk_index] = proc;
  scm_system_async_mark_for_thread (trap_thunk_store[trap_thunk_index],
                                    default_thread);
  trap_thunk_index++;
  if (trap_thunk_index > 8)
    trap_thunk_index = 1;
}

SCM
scm_trap_thunk_1 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[1]);
}
SCM
scm_trap_thunk_2 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[2]);
}
SCM
scm_trap_thunk_3 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[3]);
}
SCM
scm_trap_thunk_4 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[4]);
}
SCM
scm_trap_thunk_5 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[5]);
}
SCM
scm_trap_thunk_6 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[6]);
}
SCM
scm_trap_thunk_7 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[7]);
}
SCM
scm_trap_thunk_8 (void)
{
  return scm_call_1 (add_trap_proc, trap_func_store[8]);
}

void
gdn_trap_info_guile_init (void)
{
  list_traps_func = scm_c_public_ref ("system vm trap-state", "list-traps");
  trap_name_func = scm_c_public_ref ("system vm trap-state", "trap-name");
  trap_enabled_func =
      scm_c_public_ref ("system vm trap-state", "trap-enabled?");

  scm_c_define_gsubr ("gdn-update-traps", 0, 0, 0, scm_update_traps);
  trap_thunk_store[1] =
      scm_c_define_gsubr ("trap-thunk-1", 0, 0, 0, scm_trap_thunk_1);
  trap_thunk_store[2] =
      scm_c_define_gsubr ("trap-thunk-2", 0, 0, 0, scm_trap_thunk_2);
  trap_thunk_store[3] =
      scm_c_define_gsubr ("trap-thunk-3", 0, 0, 0, scm_trap_thunk_3);
  trap_thunk_store[4] =
      scm_c_define_gsubr ("trap-thunk-4", 0, 0, 0, scm_trap_thunk_4);
  trap_thunk_store[5] =
      scm_c_define_gsubr ("trap-thunk-5", 0, 0, 0, scm_trap_thunk_5);
  trap_thunk_store[6] =
      scm_c_define_gsubr ("trap-thunk-6", 0, 0, 0, scm_trap_thunk_6);
  trap_thunk_store[7] =
      scm_c_define_gsubr ("trap-thunk-7", 0, 0, 0, scm_trap_thunk_7);
  trap_thunk_store[8] =
      scm_c_define_gsubr ("trap-thunk-8", 0, 0, 0, scm_trap_thunk_8);
}
