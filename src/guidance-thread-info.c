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

/* One row of the Threads page: a thread of the program, as the REPL
 * describes it. */
struct _GdnThreadInfo
{
  GObject parent;

  char *name;      /* "Main thread", "Thread 2", ... */
  char *procedure; /* What the thread was started with */
  char *state;     /* "running", "paused", "finished", ... */
};

G_DEFINE_TYPE (GdnThreadInfo, gdn_thread_info, G_TYPE_OBJECT)

static void
gdn_thread_info_finalize (GObject *object)
{
  GdnThreadInfo *self = GDN_THREAD_INFO (object);

  g_free (self->name);
  g_free (self->procedure);
  g_free (self->state);
  G_OBJECT_CLASS (gdn_thread_info_parent_class)->finalize (object);
}

static void
gdn_thread_info_class_init (GdnThreadInfoClass *klass)
{
  G_OBJECT_CLASS (klass)->finalize = gdn_thread_info_finalize;
}

static void
gdn_thread_info_init (G_GNUC_UNUSED GdnThreadInfo *self)
{
}

GdnThreadInfo *
gdn_thread_info_new (const char *name, const char *procedure,
                     const char *state)
{
  GdnThreadInfo *self = g_object_new (GDN_THREAD_INFO_TYPE, NULL);

  self->name = g_strdup (name);
  self->procedure = g_strdup (procedure);
  self->state = g_strdup (state);
  return self;
}

/* ROW is a vector #(NAME PROCEDURE STATE) of strings.  Returns NULL if
 * it is not. */
GdnThreadInfo *
gdn_thread_info_new_from_scm (SCM row)
{
  if (!scm_is_vector (row) || scm_c_vector_length (row) != 3)
    return NULL;
  for (size_t i = 0; i < 3; i++)
    if (!scm_is_string (scm_c_vector_ref (row, i)))
      return NULL;

  char *name = scm_to_utf8_string (scm_c_vector_ref (row, 0));
  char *procedure = scm_to_utf8_string (scm_c_vector_ref (row, 1));
  char *state = scm_to_utf8_string (scm_c_vector_ref (row, 2));
  GdnThreadInfo *self = gdn_thread_info_new (name, procedure, state);
  free (name);
  free (procedure);
  free (state);
  return self;
}

const char *
gdn_thread_info_get_name (GdnThreadInfo *info)
{
  return info->name;
}

const char *
gdn_thread_info_get_procedure (GdnThreadInfo *info)
{
  return info->procedure;
}

const char *
gdn_thread_info_get_state (GdnThreadInfo *info)
{
  return info->state;
}
