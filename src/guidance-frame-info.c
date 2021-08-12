/* yjd_frame_info.c
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

#include "yjd_frame_info.h"

/* THIS TIME FOR REAL
 * There is a FrameInfo and each FrameInfo has a list of VarInfo
 * The FrameInfo is
 * - name
 * - source location
 
 *
 * The VarInfo is index, name, slot, representation
 * But it could be a tree, so that children could expand
 * = variables can expand into vars
 * = vectors
 * = things that are pairs can expand into car / cdr

 */

/**
 * yjd_frame_contents_get_type:
 * Initializes the YJD_FRAME_CONTENTS enum
 *
 * @return: The new enum's GType.
 */
GType
yjd_frame_contents_get_type (void)
{
  static gsize yjd_frame_contents_type = 0;

  if (g_once_init_enter (&yjd_frame_contents_type))
    {
      static const GEnumValue values[] = {
        { YJD_FRAME_CONTENTS_UNKNOWN, "YJD_FRAME_CONTENTS_UNKNOWN", "unknown" },
        { YJD_FRAME_CONTENTS_FRAME, "YJD_FRAME_CONTENTS_FRAME", "frame" },
        { YJD_FRAME_CONTENTS_ARGUMENT, "YJD_FRAME_CONTENTS_ARGUMENT", "argument" },
        { YJD_FRAME_CONTENTS_VARIABLE, "YJD_FRAME_CONTENTS_VARIABLE", "variable" },
        { 0, NULL, NULL }
      };
      GType type;

      type = g_enum_register_static ("YjdFrameContents", values);

      g_once_init_leave (&yjd_frame_contents_type, type);
    }
  return yjd_frame_contents_type;
}

/**
 * YjdFrameInfo:
 *
 * This GObject structure holds descriptive information about a Guile
 * frame or variable.
 *
 * This structure is a bit weird because we're using a GtkListView to
 * visualize a tree.  The *view* will be simple: three columns. Name,
 * Value, Description.
 *
 * But the model is complicated because each element could be a frame
 * with associated variables, or could just be a variable.
 *
 */
struct _GdnFrameInfo
{
  GObject parent;

  YjdFrameContents contents;  /** If this row holds a whole frame or just a variable */
  char *           name;      /**< The name of the frame or variable. */
  GValue           value;     /**< The value of a variable, or the pointer-packed Guile frame */
  char *           filename;  /**< For a frame, the source location if known */
  int              line;      /**< For a frame, the source location if known */
  int              col;       /**< For a frame, the source location if known */
  GHashTable *     arguments; /**< Arg name/val, if this frame is an entry point */
  GHashTable *     locals;    /** Var name/val for this frame's locals. */
};

G_DEFINE_TYPE (YjdFrameInfo, yjd_frame_info, G_TYPE_OBJECT);

enum
{
  PROP_0,
  PROP_TYPE,
  PROP_NAME,
  PROP_VALUE,
  PROP_LOCATION,
  N_PROPS
};
static GParamSpec *properties[N_PROPS] = {
  NULL,
};

static void
yjd_frame_info_get_property (GObject *object, unsigned int property_id, GValue *value, GParamSpec *pspec)
{
  YjdFrameInfo *self = YJD_FRAME_INFO (object);

  switch (property_id)
    {
    case PROP_TYPE:
      g_value_set_enum (value, self->contents);
      break;
    case PROP_NAME:
      g_value_set_string (value, self->name);
      break;
    case PROP_VALUE:
      {
        if (G_VALUE_HOLDS_POINTER (&(self->value)))
          g_value_set_string (value, g_strdup_printf ("%p", g_value_get_pointer (&(self->value))));
        else if (G_VALUE_HOLDS_INT64 (&(self->value)))
          g_value_set_string (value, g_strdup_printf ("%ld", g_value_get_int64 (&(self->value))));
        else if (G_VALUE_HOLDS_STRING (&(self->value)))
          g_value_set_string (value, g_value_dup_string (&(self->value)));
      }
      break;
    case PROP_LOCATION:
      if (self->filename)
        g_value_set_string (value, g_strdup_printf ("%s:%d:%d", self->filename, self->line, self->col));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
yjd_frame_info_finalize (GObject *object)
{
  YjdFrameInfo *self = YJD_FRAME_INFO (object);

  self->contents = YJD_FRAME_CONTENTS_UNKNOWN;
  free (self->name);
  self->name = NULL;
  g_value_unset (&(self->value));
  free (self->filename);
  self->filename = NULL;
  self->line = 0;
  self->col = 0;
  g_hash_table_unref (self->arguments);
  self->arguments = NULL;
  g_hash_table_unref (self->locals);
  self->locals = NULL;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (yjd_frame_info_parent_class)->finalize (object);
}

static void
yjd_frame_info_class_init (YjdFrameInfoClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = yjd_frame_info_finalize;
  gobj_class->get_property = yjd_frame_info_get_property;

  properties[PROP_TYPE] =
      g_param_spec_enum ("type", "type", "frame, argument or variable", YJD_FRAME_CONTENTS_TYPE,
                         YJD_FRAME_CONTENTS_UNKNOWN, G_PARAM_READABLE);
  properties[PROP_NAME] =
      g_param_spec_string ("name", "name", "name of frame or variable", NULL, G_PARAM_READABLE);
  properties[PROP_VALUE] =
      g_param_spec_string ("value", "value", "value of frame or variable", NULL, G_PARAM_READABLE);
  properties[PROP_LOCATION] = g_param_spec_string (
      "location", "location", "source code location info", NULL, G_PARAM_READABLE);
}

static void
yjd_frame_info_init (YjdFrameInfo *self)
{
}

/**
 * yjd_frame_info_new:
 *
 * This procedure creates a new Guile frame info object.  This is
 * just a container that holds information about a Guile frame, for
 * use in a GListStore.
 *
 * @name: The name of an entry
 * @enabled: If the frame is enabled.
 * @index: An integer index of the frame
 * @return: A frame info object
 */
YjdFrameInfo *
yjd_frame_info_new (YjdFrameContents contents, const char *name, GValue *value, const char *filename, int line, int col, GHashTable *arguments, GHashTable *locals)

{
  YjdFrameInfo *self = g_object_new (YJD_FRAME_INFO_TYPE, NULL);

  self->contents = contents;
  self->name = g_strdup (name);
  g_value_copy (value, &(self->value));
  self->filename = g_strdup (filename);
  self->line = line;
  self->col = col;
  self->arguments = arguments;
  g_hash_table_ref (self->arguments);
  self->locals = locals;
  g_hash_table_ref (self->locals);

  return self;
}

/* This backtrace structure is a vector where each entry is
 * - a string of frame procedure name and its arguments
 * - filename, line, col (which could be #f if unknown)
 * - a list of bindings
 *
 * Each entry in the bindings list is
 * - a key string
 * - a value string
 * - a flag which is true if this is an argument or false if a local
 */
SCM
gdn_frame_info_store_update (GListStore *store, SCM backtrace)
{
}
