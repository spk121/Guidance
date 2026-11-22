/* guidance-frame-info.c
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

#define G_LOG_USE_STRUCTURED
#include "guidance-frame-info.h"
#include "guidance-binding-info.h"

struct _GdnFrameInfo
{
  GObject parent;

  char *  name;     /**< The name of the frame or variable. */
  char *  filename; /**< For a frame, the source location if known */
  int     line;     /**< For a frame, the source location if known */
  int     column;   /**< For a frame, the source location if known */
  int     n_args;
  GPtrArray *bindings; /**< GdnBindingInfo for arguments and locals */
};

G_DEFINE_TYPE (GdnFrameInfo, gdn_frame_info, G_TYPE_OBJECT)

enum
{
  PROP_0,
  PROP_NAME,
  PROP_FILENAME,
  PROP_LINE,
  PROP_COLUMN,
  PROP_N_ARGS,
  N_PROPS
};
static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static void
gdn_frame_info_get_property (GObject *    object,
                             unsigned int property_id,
                             GValue *     value,
                             GParamSpec * pspec)
{
  GdnFrameInfo *self = GDN_FRAME_INFO (object);

  switch (property_id)
    {
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_FILENAME:
      g_value_set_string (value, self->filename);
      break;
    case PROP_LINE:
      g_value_set_int (value, self->line);
      break;
    case PROP_COLUMN:
      g_value_set_int (value, self->column);
      break;
    case PROP_N_ARGS:
      g_value_set_int (value, self->n_args);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
gdn_frame_info_finalize (GObject *object)
{
  g_assert (G_IS_OBJECT (object));
  g_assert (G_OBJECT_TYPE (object) == GDN_FRAME_INFO_TYPE);
  GdnFrameInfo *self = GDN_FRAME_INFO (object);
  g_assert (self->name != NULL);

  free (self->name);
  self->name = NULL;
  free (self->filename);
  self->filename = NULL;
  self->line = 0;
  self->column = 0;
  self->n_args = 0;
  g_clear_pointer (&self->bindings, g_ptr_array_unref);

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_frame_info_parent_class)->finalize (object);
}

static void
gdn_frame_info_class_init (GdnFrameInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = gdn_frame_info_finalize;
  gobj_class->get_property = gdn_frame_info_get_property;

  properties[PROP_NAME] = g_param_spec_string ("name", "name", "name of frame",
                                               NULL, G_PARAM_READABLE);
  properties[PROP_FILENAME] = g_param_spec_string (
      "filename", "filename", "filename", NULL, G_PARAM_READABLE);
  properties[PROP_LINE] = g_param_spec_int ("line", "line", "line", 0, G_MAXINT,
                                            0, G_PARAM_READABLE);
  properties[PROP_COLUMN] = g_param_spec_int ("column", "column", "column", 0,
                                              G_MAXINT, 0, G_PARAM_READABLE);
  properties[PROP_N_ARGS] = g_param_spec_int ("n-args", "n-args", "n-args", 0,
                                              G_MAXINT, 0, G_PARAM_READABLE);
}

static void
gdn_frame_info_init (G_GNUC_UNUSED GdnFrameInfo *self)
{
}


static GdnFrameInfo *
frame_info_new_from_scm (SCM frame)
{
  GdnFrameInfo *self = g_object_new (GDN_FRAME_INFO_TYPE, NULL);
  SCM           s_name;
  SCM           s_filename;
  SCM           s_line;
  SCM           s_column;
  SCM           s_n_args;
  SCM           s_bindings;

  s_name = scm_c_vector_ref (frame, 0);
  s_filename = scm_c_vector_ref (frame, 1);
  s_line = scm_c_vector_ref (frame, 2);
  s_column = scm_c_vector_ref (frame, 3);
  s_n_args = scm_c_vector_ref (frame, 4);
  s_bindings = scm_c_vector_ref (frame, 5);

  self->name = scm_to_utf8_string (s_name);
  self->filename = scm_to_utf8_string (s_filename);
  self->line = scm_to_int (s_line);
  self->column = scm_to_int (s_column);
  self->n_args = scm_to_int (s_n_args);
  self->bindings = gdn_binding_info_array_new_from_scm (s_bindings);

  g_log_structured (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "MESSAGE_ID",
                    "38a21f3e600d497c9194e4080769fdb0", "CODE_FILE", __FILE__,
                    "CODE_LINE", __LINE__, "CODE_FUNC", __func__, "MESSAGE",
                    "new GdnFrameInfo %p", self);
  return self;
}

GPtrArray *
gdn_frame_info_array_new_from_scm (SCM frames)
{
  g_assert (scm_is_vector (frames));

  size_t     len = scm_c_vector_length (frames);
  GPtrArray *array = g_ptr_array_new_full (len, g_object_unref);

  for (size_t i = 0; i < len; i++)
    g_ptr_array_add (array, frame_info_new_from_scm (scm_c_vector_ref (frames, i)));
  return array;
}

const char *
gdn_frame_info_get_name (GdnFrameInfo *self)
{
  return (const char *) self->name;
}

const char *
gdn_frame_info_get_filename (GdnFrameInfo *self)
{
  return (const char *) self->filename;
}

int
gdn_frame_info_get_line (GdnFrameInfo *self)
{
  return self->line;
}

int
gdn_frame_info_get_column (GdnFrameInfo *self)
{
  return self->column;
}

GPtrArray *
gdn_frame_info_get_bindings (GdnFrameInfo *self)
{
  return self->bindings;
}
