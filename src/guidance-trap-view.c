/* guidance-trap-view.c
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

#include "guidance-trap-view.h"
#include "guidance-trap-info.h"
#include <glib-unix.h>

struct _GdnTrapView
{
  GtkBox parent_instance;

  GtkScrolledWindow *  scrolled_window;
  GtkColumnView *      column_view;
  GtkColumnViewColumn *name_col;
  GtkColumnViewColumn *index_col;
  GtkColumnViewColumn *active_col;
  GtkColumnViewColumn *delete_col;

  GPtrArray * temp_store;
  GListStore *store;
};

G_DEFINE_TYPE (GdnTrapView, gdn_trap_view, GTK_TYPE_BOX)

enum
{
  ENABLE_TRAP = 0,
  DELETE_TRAP,
  N_SIGNALS
};

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
static unsigned signals[N_SIGNALS];
static SCM      scm_trap_view_type = SCM_BOOL_F;
static GMutex   lock;
static gboolean update_store (gpointer user_data);

typedef void (*factory_func_t) (GtkListItemFactory *self,
                                GtkListItem *       listitem,
                                gpointer            user_data);

static void set_column_view_model (GtkColumnView *view, GListModel *model);
static void add_column_factory (GtkColumnViewColumn *col,
                                factory_func_t       setup,
                                factory_func_t       bind,
                                factory_func_t       unbind);

static void name_setup (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        void *              user_data);
static void name_bind (GtkListItemFactory *factory,
                       GtkListItem *       list_item,
                       void *              user_data);
static void name_unbind (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         void *              user_data);
static void index_setup (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         void *              user_data);
static void index_bind (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        void *              user_data);
static void index_unbind (GtkListItemFactory *factory,
                          GtkListItem *       list_item,
                          void *              user_data);
static void active_setup (GtkListItemFactory *factory,
                          GtkListItem *       list_item,
                          void *              user_data);
static void active_bind (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         void *              user_data);
static void active_unbind (GtkListItemFactory *factory,
                           GtkListItem *       list_item,
                           void *              user_data);
static void delete_setup (GtkListItemFactory *factory,
                          GtkListItem *       list_item,
                          void *              user_data);
static void delete_bind (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         void *              user_data);
static void delete_unbind (GtkListItemFactory *factory,
                           GtkListItem *       list_item,
                           void *              user_data);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_trap_view_class_init (GdnTrapViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/trap-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnTrapView, x)

  BIND (column_view);
  BIND (name_col);
  BIND (index_col);
  BIND (active_col);
  BIND (delete_col);

#undef BIND

  signals[ENABLE_TRAP] =
      g_signal_new ("enable-trap", G_TYPE_FROM_CLASS (klass),
                    G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, 0, NULL, NULL,
                    NULL, G_TYPE_NONE, 2, G_TYPE_INT, G_TYPE_BOOLEAN);
  signals[DELETE_TRAP] =
      g_signal_new ("delete-trap", G_TYPE_FROM_CLASS (klass),
                    G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, 0, NULL, NULL,
                    NULL, G_TYPE_NONE, 1, G_TYPE_INT);
}

static void
gdn_trap_view_init (GdnTrapView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  self->temp_store = g_ptr_array_new_full (1, g_object_unref);
  self->store = g_list_store_new (GDN_TRAP_INFO_TYPE);

  set_column_view_model (self->column_view, G_LIST_MODEL (self->store));
  add_column_factory (self->name_col, name_setup, name_bind, name_unbind);
  add_column_factory (self->index_col, index_setup, index_bind, index_unbind);
  add_column_factory (self->active_col, active_setup, active_bind,
                      active_unbind);
  add_column_factory (self->delete_col, delete_setup, delete_bind,
                      delete_unbind);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static void
delete_activate (G_GNUC_UNUSED GtkButton *self,
                 G_GNUC_UNUSED gpointer   user_data)
{
#if 1
  g_debug ("activate delete");
#else
  GtkListItem *list_item = user_data;

  GObject *    obj = gtk_list_item_get_item (list_item);
  GdnTrapInfo *info = GDN_TRAP_INFO (obj);
  SCM          bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
#endif
}

static void
name_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel * label;

  label = GTK_LABEL (gtk_label_new (""));
  gtk_label_set_xalign (label, 0);
  gtk_label_set_width_chars (label, 30);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
name_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *   label;
  GdnTrapInfo *info;
  GObject *    obj;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_TRAP_INFO (obj);
  gtk_label_set_text (label, gdn_trap_info_get_name (info));
  if (label)
    gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);
}

static void
name_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel * label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
index_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
index_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *   label;
  GObject *    obj;
  GdnTrapInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_TRAP_INFO (obj);
  int   index = gdn_trap_info_get_index (info);
  char *str;
  str = g_strdup_printf ("%d", index);
  gtk_label_set_text (label, str);
  free (str);
}

static void
index_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
active_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  GtkBox *   box;
  GtkSwitch *swtch;

  box = GTK_BOX (gtk_box_new (GTK_ORIENTATION_VERTICAL, 0));
  swtch = GTK_SWITCH (gtk_switch_new ());
  gtk_box_append (box, GTK_WIDGET (swtch));
  gtk_list_item_set_child (list_item, GTK_WIDGET (box));
}

static void
active_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkBox *     box;
  GtkSwitch *  swtch;
  GObject *    obj;
  GdnTrapInfo *info;

  box = GTK_BOX (gtk_list_item_get_child (list_item));
  swtch = GTK_SWITCH (gtk_widget_get_first_child (GTK_WIDGET (box)));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_TRAP_INFO (obj);
  gtk_switch_set_active (swtch, gdn_trap_info_get_active (info));
}

static void
active_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
               G_GNUC_UNUSED GtkListItem *list_item,
               G_GNUC_UNUSED gpointer     user_data)
{
}

static void
delete_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  GtkBox *   box;
  GtkButton *button;

  box = GTK_BOX (gtk_box_new (GTK_ORIENTATION_VERTICAL, 0));
  button = GTK_BUTTON (gtk_button_new_from_icon_name ("delete"));
  gtk_box_append (box, GTK_WIDGET (button));
  gtk_list_item_set_child (list_item, GTK_WIDGET (box));

  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (delete_activate),
                    list_item);
}

static void
delete_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
             G_GNUC_UNUSED GtkListItem *list_item,
             G_GNUC_UNUSED gpointer     user_data)
{
}

static void
delete_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
               G_GNUC_UNUSED GtkListItem *list_item,
               G_GNUC_UNUSED gpointer     user_data)
{
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

/* This GSourceFunc is used on-idle to move the trap temp_store to
 * the trap GListStore that the GUI uses. This has to occur in the
 * GTK thread. */
static gboolean
update_store (gpointer user_data)
{
  GdnTrapView * self = GDN_TRAP_VIEW (user_data);
  gsize         len;
  GdnTrapInfo **array;

  g_mutex_lock (&lock);
  array = g_ptr_array_steal (self->temp_store, &len);

  /* This operation adds a reference to each entry array. */
  g_list_store_splice (self->store, 0, g_list_model_get_n_items (self->store),
                       array, len);

  /* So we can drop the original reference here. */
  for (guint i = 0; i < len; i++)
    g_object_unref (g_list_model_get_object (self->store, i));
  g_mutex_unlock (&lock);

  /* We only run this once. */
  return G_SOURCE_REMOVE;
}

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
                    factory_func_t       unbind)
{
  GtkSignalListItemFactory *factory;
  factory = GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (factory, "setup", G_CALLBACK (setup), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (bind), NULL);
  g_signal_connect (factory, "unbind", G_CALLBACK (unbind), NULL);
  gtk_column_view_column_set_factory (col, GTK_LIST_ITEM_FACTORY (factory));
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

SCM
gdn_trap_view_to_scm (GdnTrapView *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_TRAP_VIEW);
  return scm_make_foreign_object_1 (scm_trap_view_type, self);
}

/* GUILE THREAD: Since traps are stored per-thread, this procedure
 * needs to be run in the thread in which the traps have been set.
 */
static SCM
scm_update_traps_x (SCM s_self, SCM trap_list)
{
  scm_assert_foreign_object_type (scm_trap_view_type, s_self);

  g_mutex_lock (&lock);
  SCM          trap_vec = scm_vector (trap_list);
  GdnTrapView *self = scm_foreign_object_ref (s_self, 0);

  g_ptr_array_set_size (self->temp_store, 0);

  for (size_t i = 0; i < scm_c_vector_length (trap_vec); i++)
    {
      GdnTrapInfo *info = trap_info_new_from_trap_id (
          scm_to_int (scm_c_vector_ref (trap_vec, i)), 0);
      g_ptr_array_insert (self->temp_store, -1, info);
    }

  g_mutex_unlock (&lock);

  /* The GListModel can only be updated from the GTK thread, so
   * we stage the changes to the temp storage array, and update the
   * GListModel in the next available tick. */
  g_idle_add (update_store, self);

  return SCM_UNSPECIFIED;
}

void
gdn_trap_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-trap-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_trap_view_type = scm_make_foreign_object_type (name, slots, NULL);

  scm_c_define_gsubr ("gdn-update-traps!", 2, 0, 0,
                      (scm_t_subr) scm_update_traps_x);
  scm_c_export ("gdn-update-traps!", NULL);
}
