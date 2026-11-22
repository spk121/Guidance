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
#include "guidance-xgtk.h"
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
  SELECT_FRAME,
  TOP_FRAME,
  N_SIGNALS
};

typedef struct _GdnBacktraceViewAndListItem
{
  GdnBacktraceView *view;
  GtkListItem *     item;
} GdnBacktraceViewAndListItem;

typedef struct
{
  GdnBacktraceView *view;
  GPtrArray *       frames; /* of GdnFrameInfo */
} BacktraceUpdate;

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static unsigned signals[N_SIGNALS];
static SCM      get_backtrace_func = SCM_BOOL_F;
static SCM      scm_backtrace_view_type;

typedef void (*factory_func_t) (GtkListItemFactory *self,
                                GtkListItem *       listitem,
                                gpointer            user_data);
static void set_column_view_model (GtkColumnView *view, GListModel *model);
static void add_column_factory (GtkColumnViewColumn *col,
                                factory_func_t       setup,
                                factory_func_t       bind,
                                factory_func_t       unbind,
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
static void show_bindings (GdnBacktraceView *self, GdnFrameInfo *info);
static gboolean apply_backtrace_update (gpointer user_data);
static void     backtrace_update_free (gpointer user_data);

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
  /* The operator chose a frame; its index, innermost first. */
  signals[SELECT_FRAME] = g_signal_new ("select-frame", G_TYPE_FROM_CLASS (klass),
                                        G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                                        G_TYPE_NONE, 1, G_TYPE_INT);
  /* A new backtrace arrived; the innermost frame's location. */
  signals[TOP_FRAME] = g_signal_new ("top-frame", G_TYPE_FROM_CLASS (klass),
                                     G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                                     G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT,
                                     G_TYPE_INT);
}

static void
gdn_backtrace_view_init (GdnBacktraceView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_BACKTRACE_VIEW);

  self->frames = g_list_store_new (GDN_FRAME_INFO_TYPE);
  self->bindings = g_list_store_new (GDN_BINDING_INFO_TYPE);

  set_column_view_model (self->stack_column_view, G_LIST_MODEL (self->frames));
  set_column_view_model (self->variables_column_view,
                         G_LIST_MODEL (self->bindings));
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
frame_button_clicked (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
  GdnBacktraceViewAndListItem *bvli = user_data;
  GdnBacktraceView *           self = bvli->view;
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_BACKTRACE_VIEW);
  GdnFrameInfo *info = gtk_list_item_get_item (bvli->item);
  g_assert_cmpuint (G_OBJECT_TYPE (info), ==, GDN_FRAME_INFO_TYPE);
  show_bindings (self, info);
  g_signal_emit (self, signals[SELECT_FRAME], 0,
                 (int) gtk_list_item_get_position (bvli->item));
}

/* This GtkButton::clicked handler */
static void
location_button_clicked (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
  GdnBacktraceViewAndListItem *bvli = user_data;
  GdnFrameInfo *               info = gtk_list_item_get_item (bvli->item);

  const char *filename = gdn_frame_info_get_filename (info);

  if (filename != NULL)
    {
      int line = gdn_frame_info_get_line (info);
      int col = gdn_frame_info_get_column (info);
      /* G_TYPE_STRING signal arguments are copied by the signal
       * system, so there is nothing to free here. */
      g_signal_emit (bvli->view, signals[LOCATION], 0, filename, line, col);
    }
}

static void
frame_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  g_assert_cmpuint (G_OBJECT_TYPE (user_data), ==, GDN_TYPE_BACKTRACE_VIEW);

  GtkButton *button;
  GtkLabel * label;

  button = GTK_BUTTON (gtk_button_new_with_label (NULL));
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_xalign (label, 0);
  gtk_label_set_width_chars (label, 24);
  gtk_list_item_set_child (list_item, GTK_WIDGET (button));

  GdnBacktraceViewAndListItem *bvli;
  bvli = create_pair (user_data, list_item);
  g_signal_connect_data (G_OBJECT (button), "clicked",
                         G_CALLBACK (frame_button_clicked), bvli,
                         free_pair_closure_notify, 0);
}

static void
frame_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;
  GdnFrameInfo *info;
  GObject *     obj;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = GTK_LABEL (gtk_button_get_child (button));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_FRAME_INFO (obj);
  gtk_label_set_text (label, gdn_frame_info_get_name (info));
  if (label)
    gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);
}

static void
frame_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_text (label, "");
}

static void
location_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
                GtkListItem *                     list_item,
                G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *button;
  GtkLabel * label;

  button = GTK_BUTTON (gtk_button_new_with_label (NULL));
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (button));

  GdnBacktraceViewAndListItem *bvli;
  bvli = create_pair (user_data, list_item);
  g_signal_connect_data (G_OBJECT (button), "clicked",
                         G_CALLBACK (location_button_clicked), bvli,
                         free_pair_closure_notify, 0);
}

static void
location_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
               GtkListItem *                     list_item,
               G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;
  GObject *     obj;
  GdnFrameInfo *info;
  char *        location;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = GTK_LABEL (gtk_button_get_child (button));
  obj = gtk_list_item_get_item (list_item);
  if (G_IS_OBJECT (label) && G_OBJECT_TYPE (label) == GTK_TYPE_LABEL)
    {

      info = GDN_FRAME_INFO (obj);
      const char *filename = gdn_frame_info_get_filename (info);
      if (filename != NULL && *filename != '\0')
        {
          char *base = g_path_get_basename (filename);
          location = g_strdup_printf ("%s:%d", base,
                                      gdn_frame_info_get_line (info));
          gtk_label_set_text (label, location);
          gtk_widget_set_tooltip_text (GTK_WIDGET (label), filename);
          g_free (location);
          g_free (base);
        }
      else
        {
          gtk_label_set_text (label, "N/A");
          gtk_widget_set_sensitive (GTK_WIDGET (button), FALSE);
        }
    }
}

static void
location_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
                 GtkListItem *                     list_item,
                 G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *   button;
  GtkLabel *    label;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_text (label, "");
}

static void
type_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
type_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           G_GNUC_UNUSED gpointer            user_data)
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
type_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *    label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
name_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
name_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           G_GNUC_UNUSED gpointer            user_data)
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
name_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *    label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
representation_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
                      GtkListItem *                     list_item,
                      G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
representation_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
                     GtkListItem *                     list_item,
                     G_GNUC_UNUSED gpointer            user_data)
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
representation_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
                       GtkListItem *                     list_item,
                       G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *    label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
value_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
value_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
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
value_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *    label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
info_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
info_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           G_GNUC_UNUSED gpointer            user_data)
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
info_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
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
                    factory_func_t       setup,
                    factory_func_t       bind,
                    factory_func_t       unbind,
                    gpointer             user_data)
{
  GtkSignalListItemFactory *factory;
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (factory, "setup", G_CALLBACK (setup), user_data);
  g_signal_connect (factory, "bind", G_CALLBACK (bind), user_data);
  g_signal_connect (factory, "unbind", G_CALLBACK (unbind), user_data);
  gtk_column_view_column_set_factory (col, GTK_LIST_ITEM_FACTORY (factory));
}

/* GTK THREAD: Replaces the bindings list with those of INFO, or clears
 * it if INFO is NULL. */
static void
show_bindings (GdnBacktraceView *self, GdnFrameInfo *info)
{
  guint n_old = g_list_model_get_n_items (G_LIST_MODEL (self->bindings));

  if (info == NULL || gdn_frame_info_get_bindings (info) == NULL)
    {
      g_list_store_remove_all (self->bindings);
      return;
    }
  GPtrArray *bindings = gdn_frame_info_get_bindings (info);
  g_list_store_splice (self->bindings, 0, n_old, bindings->pdata,
                       bindings->len);
}

/* GTK THREAD: Installs a new backtrace and shows the variables of the
 * innermost frame. */
static gboolean
apply_backtrace_update (gpointer user_data)
{
  BacktraceUpdate * u = user_data;
  GdnBacktraceView *self = u->view;

  g_list_store_splice (self->frames, 0,
                       g_list_model_get_n_items (G_LIST_MODEL (self->frames)),
                       u->frames->pdata, u->frames->len);
  show_bindings (self, u->frames->len > 0 ? u->frames->pdata[0] : NULL);
  if (u->frames->len > 0)
    {
      GdnFrameInfo *top = u->frames->pdata[0];
      const char *  file = gdn_frame_info_get_filename (top);
      if (file != NULL && *file != '\0')
        g_signal_emit (self, signals[TOP_FRAME], 0, file,
                       gdn_frame_info_get_line (top),
                       gdn_frame_info_get_column (top));
    }
  return G_SOURCE_REMOVE;
}

static void
backtrace_update_free (gpointer user_data)
{
  BacktraceUpdate *u = user_data;
  g_object_unref (u->view);
  g_ptr_array_unref (u->frames);
  g_free (u);
}

static GdnBacktraceViewAndListItem *
create_pair (GdnBacktraceView *view, GtkListItem *item)
{
  GdnBacktraceViewAndListItem *bvli;

  g_assert_cmpuint (G_OBJECT_TYPE (view), ==, GDN_TYPE_BACKTRACE_VIEW);
  /* Since GTK 4.12, column view cells are a GtkListItem subclass. */
  g_assert (GTK_IS_LIST_ITEM (item));

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

SCM
gdn_backtrace_view_to_scm (GdnBacktraceView *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_BACKTRACE_VIEW);
  return scm_make_foreign_object_1 (scm_backtrace_view_type, self);
}

static SCM
scm_update_backtrace_x (SCM s_self, SCM s_frames, SCM s_top_frame)
{
  scm_assert_foreign_object_type (scm_backtrace_view_type, s_self);
  if (SCM_UNBNDP (s_top_frame))
    s_top_frame = SCM_BOOL_F;
  /* S_FRAMES is a list of frames, innermost first, or #f. */
  SCM frames = scm_call_2 (get_backtrace_func, s_frames, s_top_frame);
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

  /* This is called from the Guile thread.  Build the GObjects here,
   * where reading the SCM data is natural, then hand them to the GTK
   * thread, which is the only one allowed to touch the list stores. */
  GdnBacktraceView *self = scm_foreign_object_ref (s_self, 0);
  BacktraceUpdate * u = g_new0 (BacktraceUpdate, 1);
  u->view = g_object_ref (self);
  u->frames = gdn_frame_info_array_new_from_scm (frames);
  xgtk_invoke_on_main (apply_backtrace_update, u, backtrace_update_free);
  return SCM_UNSPECIFIED;
}

void
gdn_backtrace_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-backtrace-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_backtrace_view_type = scm_make_foreign_object_type (name, slots, NULL);

  get_backtrace_func =
      scm_variable_ref (scm_c_public_lookup ("gdn lib", "gdn-get-backtrace"));

  scm_c_define_gsubr ("gdn-update-backtrace!", 2, 1, 0,
                      (scm_t_subr) scm_update_backtrace_x);
  scm_c_export ("gdn-update-backtrace!", NULL);
}
