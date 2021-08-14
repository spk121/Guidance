/* guidance-thread-info.c
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

#include "guidance-thread-info.h"

/*
 * This GObject structure holds descriptive information about a Guile
 * thread.
 *
 */
struct _GdnThreadInfo
{
  GObject parent;

  guint64  pack;
  char *   name;
  gboolean active;
  gboolean current;
};

G_DEFINE_TYPE (GdnThreadInfo, gdn_thread_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_PACK,
  PROP_NAME,
  PROP_ACTIVE,
  PROP_CURRENT,
  N_PROPS
};
static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static void
gdn_thread_info_get_property (GObject *    object,
                              unsigned int property_id,
                              GValue *     value,
                              GParamSpec * pspec)
{
  GdnThreadInfo *self = GDN_THREAD_INFO (object);

  switch (property_id)
    {
    case PROP_PACK:
      g_value_set_uint64 (value, self->pack);
      break;
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_ACTIVE:
      g_value_set_boolean (value, self->active);
      break;
    case PROP_CURRENT:
      g_value_set_boolean (value, self->current);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_thread_info_finalize (GObject *object)
{
  GdnThreadInfo *self = GDN_THREAD_INFO (object);
  self->pack = 0;
  free (self->name);
  self->name = NULL;
  self->active = FALSE;
  self->current = FALSE;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_thread_info_parent_class)->finalize (object);
}

static void
gdn_thread_info_class_init (GdnThreadInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_thread_info_finalize;
  gobj_class->get_property = gdn_thread_info_get_property;

  properties[PROP_PACK] = g_param_spec_uint64 (
      "pack", "pack", "pack", 0, G_MAXUINT64, 0, G_PARAM_READABLE);
  properties[PROP_NAME] =
      g_param_spec_string ("name", "name", "thread name", NULL, G_PARAM_READABLE);
  properties[PROP_ACTIVE] =
      g_param_spec_boolean ("active", "active", "active thread", TRUE, G_PARAM_READABLE);
  properties[PROP_CURRENT] =
      g_param_spec_boolean ("current", "current", "current thread", TRUE, G_PARAM_READABLE);
}

static void
gdn_thread_info_init (G_GNUC_UNUSED GdnThreadInfo *self)
{
}

static GdnThreadInfo *
thread_info_new_from_scm (SCM thread)
{
  const SCM format_string = scm_from_utf8_string ("~A");

  GdnThreadInfo *self;
  SCM            current_thread;
  char *         name;
  int            active;
  int            current;

  self = g_object_new (GDN_THREAD_INFO_TYPE, NULL);
  current_thread = scm_current_thread ();
  name = scm_to_utf8_string (
      scm_simple_format (SCM_BOOL_F, format_string, scm_list_1 (thread)));
  active = scm_is_false (scm_thread_exited_p (thread));
  current = scm_is_true (scm_eqv_p (thread, current_thread));
  self->pack = SCM_UNPACK (thread);
  self->name = g_strdup (name);
  self->active = active;
  self->current = current;

  return self;
}

void
gdn_thread_info_store_update (GListStore *store)
{
  SCM all_threads = scm_vector (scm_all_threads ());

  g_list_store_remove_all (store);

  for (size_t i = 0; i < scm_c_vector_length (all_threads); i++)
    {
      SCM            thrd;
      GdnThreadInfo *info;

      thrd = scm_c_vector_ref (all_threads, i);
      info = thread_info_new_from_scm (thrd);
      g_list_store_append (store, info);
    }
}
