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

struct _GdnBacktraceView
{
  GtkBox parent_instance;

  GtkScrolledWindow *  stack_window;
  GtkColumnView *      stack_column_view;
  GtkColumnViewColumn *stack_frame_column;
  GtkColumnViewColumn *stack_location_column;

  GtkScrolledWindow *  variables_window;
  GtkColumnView *      variables_column_view;
  GtkColumnViewColumn *variables_type_column;
  GtkColumnViewColumn *variables_name_column;
  GtkColumnViewColumn *variables_representation_column;
  GtkColumnViewColumn *variables_value_column;
  GtkColumnViewColumn *variables_info_column;

  GListStore *frames;
  GListStore *bindings;
};

G_DEFINE_TYPE (GdnBacktraceView, gdn_backtrace_view, GTK_TYPE_BOX)

enum
{
  LOCATION = 0,
  N_SIGNALS
};

typedef struct _GdnBacktraceViewAndListItem
{
  GdnBacktraceView *view;
  GtkListItem *     item;
} GdnBacktraceViewAndListItem;

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static SCM      get_backtrace_func = SCM_BOOL_F;
static unsigned signals[N_SIGNALS];

typedef void (*factory_func_t) (GtkSignalListItemFactory *self,
                                GtkListItem *             listitem,
                                gpointer                  user_data);
static void set_column_view_model (GtkColumnView *view, GListModel *model);
static void add_column_factory (GtkColumnViewColumn *col,
                                factory_func_t *     setup,
                                factory_func_t *     bind,
                                factory_func_t *     unbind,
                                gpointer             user_data);

static void frame_button_clicked (GtkButton *self, gpointer user_data);
static void location_button_clicked (GtkButton *self, gpointer user_data);

static void frame_setup (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         gpointer            user_data);
static void frame_bind (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        gpointer            user_data);
static void frame_unbind (GtkListItemFactory *factory,
                          GtkListItem *       list_item,
                          gpointer            user_data);
static void location_setup (GtkListItemFactory *factory,
                            GtkListItem *       list_item,
                            gpointer            user_data);
static void location_bind (GtkListItemFactory *factory,
                           GtkListItem *       list_item,
                           gpointer            user_data);
static void location_unbind (GtkListItemFactory *factory,
                             GtkListItem *       list_item,
                             gpointer            user_data);
static void type_setup (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        gpointer            user_data);
static void type_bind (GtkListItemFactory *factory,
                       GtkListItem *       list_item,
                       gpointer            user_data);
static void type_unbind (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         gpointer            user_data);
static void name_setup (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        gpointer            user_data);
static void name_bind (GtkListItemFactory *factory,
                       GtkListItem *       list_item,
                       gpointer            user_data);
static void name_unbind (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         gpointer            user_data);
static void representation_setup (GtkListItemFactory *factory,
                                  GtkListItem *       list_item,
                                  gpointer            user_data);
static void representation_bind (GtkListItemFactory *factory,
                                 GtkListItem *       list_item,
                                 gpointer            user_data);
static void representation_unbind (GtkListItemFactory *factory,
                                   GtkListItem *       list_item,
                                   gpointer            user_data);
static void value_setup (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         gpointer            user_data);
static void value_bind (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        gpointer            user_data);
static void value_unbind (GtkListItemFactory *factory,
                          GtkListItem *       list_item,
                          gpointer            user_data);
static void info_setup (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        gpointer            user_data);
static void info_bind (GtkListItemFactory *factory,
                       GtkListItem *       list_item,
                       gpointer            user_data);
static void info_unbind (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         gpointer            user_data);

static GdnBacktraceViewAndListItem *create_pair (GdnBacktraceView *view,
                                                 GtkListItem *     item);
static void free_pair (GdnBacktraceViewAndListItem *bvli);
static void free_pair_closure_notify (gpointer data, GClosure *closure);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_backtrace_view_class_init (GdnBacktraceViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/backtrace-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnBacktraceView, x)
  BIND (stack_window);
  BIND (stack_column_view);
  BIND (stack_frame_column);
  BIND (stack_location_column);
  BIND (variables_window);
  BIND (variables_column_view);
  BIND (variables_type_column);
  BIND (variables_name_column);
  BIND (variables_representation_column);
  BIND (variables_value_column);
  BIND (variables_info_column);
#undef BIND

  signals[LOCATION] = g_signal_new ("location", G_TYPE_FROM_CLASS (klass),
                                    G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, 0,
                                    NULL, NULL, NULL, G_TYPE_NONE, 3,
                                    G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT);
}

static void
gdn_backtrace_view_init (GdnBacktraceView *self)
{
  gtk_widget_init_template (self);

  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_BACKTRACE_VIEW);

  self->frames = g_list_store_new (GDN_FRAME_INFO_TYPE);
  self->bindings = g_list_store_new (GDN_BINDING_INFO_TYPE);

  set_column_view_model (self->stack_column_view, self->frames);
  set_column_view_model (self->variables_column_view, self->bindings);
  add_column_factory (self->stack_frame_column, frame_setup, frame_bind,
                      frame_unbind, self);
  add_column_factory (self->stack_location_column, location_setup,
                      location_bind, location_unbind, self);
  add_column_factory (self->variables_type_column, type_setup, type_bind,
                      type_unbind, self);
  add_column_factory (self->variables_name_column, name_setup, name_bind,
                      name_unbind, self);
  add_column_factory (self->variables_representation_column,
                      representation_setup, representation_bind,
                      representation_unbind, self);
  add_column_factory (self->variables_value_column, value_setup, value_bind,
                      value_unbind, self);
  add_column_factory (self->variables_info_column, info_setup, info_bind,
                      info_unbind, self);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

/* This GtkButton::clicked handler updates GdnBacktraceView.bindings
 * based on which of the frame info entries was selected */
static void
frame_button_clicked (GtkButton *button, gpointer user_data)
{
  GdnBacktraceViewAndListItem *bvli = user_data;
  GdnBacktraceView *           self = bvli->view;
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_BACKTRACE_VIEW);
  GdnFrameInfo *info = gtk_list_item_get_item (bvli->item);
  g_assert_cmpuint (G_OBJECT_TYPE (info), ==, GDN_FRAME_INFO_TYPE);
  SCM bindings = SCM_PACK (gdn_frame_info_get_bindings (info));

  gdn_binding_info_list_store_update_all (self->bindings, bindings);
}

/* This GtkButton::clicked handler */
static void
location_button_clicked (GtkButton *button, gpointer user_data)
{
  GdnBacktraceViewAndListItem *bvli = user_data;
  GdnBacktraceView *           self = bvli->view;
  GdnFrameInfo *               info = gtk_list_item_get_item (bvli->item);

  const char *filename = gdn_frame_info_get_filename (info);

  if (filename != NULL)
    {
      int line = gdn_frame_info_get_line (info);
      int col = gdn_frame_info_get_column (info);
      // FIXME: do I need to strdup here. How do I free?
      g_signal_emit (bvli->view, signals[LOCATION], 0, g_strdup (filename),
                     line, col);
    }
}

static void
frame_setup (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  g_assert_cmpuint (G_OBJECT_TYPE (user_data), ==, GDN_TYPE_BACKTRACE_VIEW);

  GtkButton *button;
  GtkLabel * label;

  button = gtk_button_new_with_label (NULL);
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_xalign (label, 0);
  gtk_label_set_width_chars (label, 30);
  gtk_list_item_set_child (list_item, button);

  GdnBacktraceViewAndListItem *bvli;
  bvli = create_pair (user_data, list_item);
  g_signal_connect_data (G_OBJECT (button), "clicked",
                         G_CALLBACK (frame_button_clicked), bvli,
                         free_pair_closure_notify, 0);
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

  GdnBacktraceViewAndListItem *bvli;
  bvli = create_pair (user_data, list_item);
  g_signal_connect_data (G_OBJECT (button), "clicked",
                         G_CALLBACK (location_button_clicked), bvli,
                         free_pair_closure_notify, 0);
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
      if (gdn_frame_info_get_filename (info) != NULL)
        {
          location = g_strdup_printf (
              "%s:%d:%d", gdn_frame_info_get_filename (info),
              gdn_frame_info_get_line (info), gdn_frame_info_get_column (info));
          gtk_label_set_text (label, location);
          g_free (location);
        }
      else
        {
          gtk_label_set_text (label, "N/A");
          gtk_widget_set_sensitive (button, FALSE);
        }
    }
}

static void
location_unbind (GtkListItemFactory *factory,
                 GtkListItem *       list_item,
                 gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;

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

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

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
                    factory_func_t *     unbind,
                    gpointer             user_data)
{
  GtkSignalListItemFactory *factory;
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (factory, "setup", G_CALLBACK (setup), user_data);
  g_signal_connect (factory, "bind", G_CALLBACK (bind), user_data);
  g_signal_connect (factory, "unbind", G_CALLBACK (unbind), user_data);
  gtk_column_view_column_set_factory (col, GTK_LIST_ITEM_FACTORY (factory));
}

static GdnBacktraceViewAndListItem *
create_pair (GdnBacktraceView *view, GtkListItem *item)
{
  GdnBacktraceViewAndListItem *bvli;

  g_assert_cmpuint (G_OBJECT_TYPE (view), ==, GDN_TYPE_BACKTRACE_VIEW);
  g_assert_cmpuint (G_OBJECT_TYPE (item), ==, GTK_TYPE_LIST_ITEM);

  bvli = g_new0 (GdnBacktraceViewAndListItem, 1);
  bvli->view = g_object_ref (view);
  bvli->item = g_object_ref (item);
  return bvli;
}

static void
free_pair (GdnBacktraceViewAndListItem *bvli)
{
  g_object_unref (bvli->view);
  g_object_unref (bvli->item);
  free (bvli);
}

static void
free_pair_closure_notify (gpointer data, G_GNUC_UNUSED GClosure *closure)
{
  free_pair (data);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

static SCM scm_backtrace_view_type;

SCM
gdn_backtrace_view_to_scm (GdnBacktraceView *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_BACKTRACE_VIEW);
  return scm_make_foreign_object_1 (scm_backtrace_view_type, self);
}

static SCM
scm_update_backtrace_x (SCM s_self, SCM s_frame)
{
  scm_assert_foreign_object_type (scm_backtrace_view_type, s_self);
  if (SCM_UNBNDP (s_frame))
    s_frame = SCM_BOOL_F;
  SCM frames = scm_call_1 (get_backtrace_func, s_frame);
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

  GdnBacktraceView *self = scm_foreign_object_ref (s_self, 0);
  gdn_frame_info_store_update (self->frames, frames);
  return SCM_UNSPECIFIED;
}

void
gdn_backtrace_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-backtrace-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_backtrace_view_type = scm_make_foreign_object_type (name, slots, NULL);

  get_backtrace_func = scm_variable_ref (scm_c_lookup ("gdn-get-backtrace"));

  scm_c_define_gsubr ("gdn-update-backtrace!", 1, 1, 0,
                      (scm_t_subr) scm_update_backtrace_x);
}
