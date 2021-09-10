/* guidance-backtrace-view.c
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

#include "guidance-backtrace-view.h"
#include "guidance-binding-info.h"
#include "guidance-frame-info.h"

typedef void (*factory_func_t) (GtkSignalListItemFactory *self,
                                GtkListItem *             listitem,
                                gpointer                  user_data);

static GtkColumnView *      _stack_view = NULL;
static GtkColumnViewColumn *_frame_col = NULL;
static GtkColumnViewColumn *_location_col = NULL;
static GtkColumnView *      _variable_view = NULL;
static GtkColumnViewColumn *_type_col = NULL;
static GtkColumnViewColumn *_name_col = NULL;
static GtkColumnViewColumn *_representation_col = NULL;
static GtkColumnViewColumn *_value_col = NULL;
static GtkColumnViewColumn *_info_col = NULL;
static GdnFrameInfo *       _frames = NULL;
static GdnBindingInfo *     _bindings = NULL;
static SCM                  _get_backtrace_func = SCM_BOOL_F;
static GtkWidget *          _main_stack = NULL;

static void
set_column_view_model (GtkColumnView *view, GListModel *model)
{
  GtkNoSelection *nosel_model;
  nosel_model =
      gtk_no_selection_new (G_LIST_MODEL (g_object_ref (G_OBJECT (model))));
  gtk_column_view_set_model (view, GTK_SELECTION_MODEL (nosel_model));
}

static void
add_column_factory (GtkColumnViewColumn *col,
                    factory_func_t *     setup,
                    factory_func_t *     bind,
                    factory_func_t *     unbind)
{
  GtkSignalListItemFactory *factory;
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (factory, "setup", G_CALLBACK (setup), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (bind), NULL);
  g_signal_connect (factory, "unbind", G_CALLBACK (unbind), NULL);
  gtk_column_view_column_set_factory (col, GTK_LIST_ITEM_FACTORY (factory));
}

////////////////////////////////////////////////////////////////
static void
frame_button_activate (GtkButton *self, gpointer user_data)
{
  GtkListItem *list_item = user_data;

  GObject *     obj = gtk_list_item_get_item (list_item);
  GdnFrameInfo *info = GDN_FRAME_INFO (obj);
  SCM           bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
}

static void
location_button_activate (GtkButton *self, gpointer user_data)
{
  GtkListItem *list_item = user_data;

  GObject *     obj = gtk_list_item_get_item (list_item);
  GdnFrameInfo *info = GDN_FRAME_INFO (obj);

  const char *filename = gdn_frame_info_get_filename (info);
  int         line = gdn_frame_info_get_line (info);
  int         col = gdn_frame_info_get_column (info);
  GtkWidget * wigz = gtk_stack_get_child_by_name (_main_stack, "source");
  if (wigz != NULL)
    gtk_stack_set_visible_child (_main_stack, wigz);
  gdn_source_view_show_location (filename, line, col);
}

////////////////////////////////////////////////////////////////

static void
frame_setup (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkButton *button;
  GtkLabel * label;

  button = gtk_button_new_with_label (NULL);
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_xalign (label, 0);
  gtk_label_set_width_chars (label, 30);
  gtk_list_item_set_child (list_item, button);

  g_signal_connect (G_OBJECT (button), "clicked",
                    G_CALLBACK (frame_button_activate), list_item);
}

static void
frame_bind (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;
  GdnFrameInfo *info;
  GObject *     obj;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = gtk_button_get_child (button);
  obj = gtk_list_item_get_item (list_item);
  info = GDN_FRAME_INFO (obj);
  gtk_label_set_text (label, gdn_frame_info_get_name (info));
  if (label)
    gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);
}

static void
frame_unbind (GtkListItemFactory *factory,
              GtkListItem *       list_item,
              gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;
  GdnFrameInfo *info;
  GObject *     obj;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = gtk_button_get_child (button);
  gtk_label_set_text (label, "");
}

static void
location_setup (GtkListItemFactory *factory,
                GtkListItem *       list_item,
                gpointer            user_data)
{
  GtkButton *button;
  GtkLabel * label;

  button = gtk_button_new_with_label (NULL);
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, button);

  g_signal_connect (G_OBJECT (button), "clicked",
                    G_CALLBACK (location_button_activate), list_item);
}

static void
location_bind (GtkListItemFactory *factory,
               GtkListItem *       list_item,
               gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;
  char *        location;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = gtk_button_get_child (button);
  obj = gtk_list_item_get_item (list_item);
  if (G_IS_OBJECT (label) && G_OBJECT_TYPE (label) == GTK_TYPE_LABEL)
    {

      info = GDN_FRAME_INFO (obj);
      location = g_strdup_printf (
          "%s:%d:%d", gdn_frame_info_get_filename (info),
          gdn_frame_info_get_line (info), gdn_frame_info_get_column (info));
      gtk_label_set_text (label, location);
      g_free (location);
    }
}

static void
location_unbind (GtkListItemFactory *factory,
                 GtkListItem *       list_item,
                 gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = gtk_button_get_child (button);
  gtk_label_set_text (label, "");
}

static void
type_setup (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, label);
}

static void
type_bind (GtkListItemFactory *factory,
           GtkListItem *       list_item,
           gpointer            user_data)
{
  GtkLabel *      label;
  GObject *       obj;
  GdnBindingInfo *info;
  char *          location;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_BINDING_INFO (obj);
  if (gdn_binding_info_get_argument (info))
    gtk_label_set_text (label, "A");
  else
    gtk_label_set_text (label, "");
}

static void
type_unbind (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
name_setup (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, label);
}

static void
name_bind (GtkListItemFactory *factory,
           GtkListItem *       list_item,
           gpointer            user_data)
{
  GtkLabel *      label;
  GObject *       obj;
  GdnBindingInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_BINDING_INFO (obj);
  gtk_label_set_text (label, gdn_binding_info_get_name (info));
}

static void
name_unbind (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
representation_setup (GtkListItemFactory *factory,
                      GtkListItem *       list_item,
                      gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, label);
}

static void
representation_bind (GtkListItemFactory *factory,
                     GtkListItem *       list_item,
                     gpointer            user_data)
{
  GtkLabel *      label;
  GObject *       obj;
  GdnBindingInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_BINDING_INFO (obj);
  gtk_label_set_text (label, gdn_binding_info_get_representation (info));
}

static void
representation_unbind (GtkListItemFactory *factory,
                       GtkListItem *       list_item,
                       gpointer            user_data)
{
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
value_setup (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, label);
}

static void
value_bind (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkLabel *      label;
  GObject *       obj;
  GdnBindingInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_BINDING_INFO (obj);
  gtk_label_set_text (label, gdn_binding_info_get_value (info));
}

static void
value_unbind (GtkListItemFactory *factory,
              GtkListItem *       list_item,
              gpointer            user_data)
{
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
info_setup (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, label);
}

static void
info_bind (GtkListItemFactory *factory,
           GtkListItem *       list_item,
           gpointer            user_data)
{
  GtkLabel *      label;
  GObject *       obj;
  GdnBindingInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_BINDING_INFO (obj);
  gtk_label_set_text (label, gdn_binding_info_get_extra (info));
}

static void
info_unbind (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

////////////////////////////////////////////////////////////////
void
gdn_backtrace_view_init (GtkColumnView *      stack_view,
                         GtkColumnViewColumn *frame_col,
                         GtkColumnViewColumn *location_col,
                         GtkColumnView *      variable_view,
                         GtkColumnViewColumn *type_col,
                         GtkColumnViewColumn *name_col,
                         GtkColumnViewColumn *representation_col,
                         GtkColumnViewColumn *value_col,
                         GtkColumnViewColumn *info_col,
                         GtkWidget *          main_stack)
{
  _stack_view = g_object_ref (stack_view);
  _frame_col = g_object_ref (frame_col);
  _location_col = g_object_ref (location_col);
  _variable_view = g_object_ref (variable_view);
  _type_col = g_object_ref (type_col);
  _name_col = g_object_ref (name_col);
  _representation_col = g_object_ref (representation_col);
  _value_col = g_object_ref (_value_col);
  _info_col = g_object_ref (_info_col);
  _main_stack = main_stack;

  _frames = g_list_store_new (GDN_FRAME_INFO_TYPE);
  _bindings = gdn_binding_info_get_list_store ();

  set_column_view_model (_stack_view, _frames);
  set_column_view_model (_variable_view, _bindings);
  add_column_factory (_frame_col, frame_setup, frame_bind, frame_unbind);
  add_column_factory (_location_col, location_setup, location_bind,
                      location_unbind);
  add_column_factory (_type_col, type_setup, type_bind, type_unbind);
  add_column_factory (_name_col, name_setup, name_bind, name_unbind);
  add_column_factory (_representation_col, representation_setup,
                      representation_bind, representation_unbind);
  add_column_factory (_value_col, value_setup, value_bind, value_unbind);
  add_column_factory (_info_col, info_setup, info_bind, info_unbind);
}

////////////////////////////////////////////////////////////////

static SCM
scm_update_backtrace (SCM frame)
{
  SCM frames = scm_call_1 (_get_backtrace_func, frame);
#ifndef G_DISABLE_ASSERT
  g_assert (scm_is_vector (frames));
  for (size_t i = 0; i < scm_c_vector_length (frames); i++)
    {
      SCM entry = scm_c_vector_ref (frames, i);
      g_assert (scm_is_vector (entry));
      g_assert_cmpint (scm_c_vector_length (entry), ==, 6);
      g_assert (scm_is_string (scm_c_vector_ref (entry, 0)));
      g_assert (scm_is_string (scm_c_vector_ref (entry, 1)));
      g_assert (scm_is_integer (scm_c_vector_ref (entry, 2)));
      g_assert (scm_is_integer (scm_c_vector_ref (entry, 3)));
      g_assert (scm_is_integer (scm_c_vector_ref (entry, 4)));
      g_assert (scm_is_vector (scm_c_vector_ref (entry, 5)));
    }
#endif

  gdn_frame_info_store_update (_frames, frames);
  return SCM_UNSPECIFIED;
}

void
gdn_backtrace_view_guile_init (void)
{
  _get_backtrace_func = scm_variable_ref (scm_c_lookup ("gdn-get-backtrace"));
  scm_c_define_gsubr ("gdn-update-backtrace", 1, 0, 0, scm_update_backtrace);
}
