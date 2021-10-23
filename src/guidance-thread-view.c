/* guidance-thread-view.c
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

#include "guidance-thread-view.h"
#include "guidance-emoji.h"
#include "guidance-thread-info.h"
#include <glib-unix.h>

struct _GdnThreadView
{
  GtkBox parent_instance;

  GtkScrolledWindow *  scrolled_window;
  GtkColumnView *      column_view;
  GtkColumnViewColumn *name_col;
  GtkColumnViewColumn *emoji_col;
  GtkColumnViewColumn *active_col;

  GPtrArray * temp_store;
  GListStore *store;
};

G_DEFINE_TYPE (GdnThreadView, gdn_thread_view, GTK_TYPE_BOX)

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static SCM scm_thread_view_type = SCM_BOOL_F;
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
static void emoji_setup (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         void *              user_data);
static void emoji_bind (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        void *              user_data);
static void emoji_unbind (GtkListItemFactory *factory,
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

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_thread_view_class_init (GdnThreadViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/thread-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnThreadView, x)

  BIND (column_view);
  BIND (name_col);
  BIND (emoji_col);
  BIND (active_col);

#undef BIND
}

static void
gdn_thread_view_init (GdnThreadView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));
  self->temp_store = g_ptr_array_new_full (1, g_object_unref);
  self->store = g_list_store_new (GDN_THREAD_INFO_TYPE);

  set_column_view_model (self->column_view, G_LIST_MODEL (self->store));
  add_column_factory (self->name_col, name_setup, name_bind, name_unbind);
  add_column_factory (self->emoji_col, emoji_setup, emoji_bind, emoji_unbind);
  add_column_factory (self->active_col, active_setup, active_bind,
                      active_unbind);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static void
name_activate (G_GNUC_UNUSED GtkButton *self, G_GNUC_UNUSED gpointer user_data)
{
#if 1
  g_debug ("activate thread");
#else
  GtkListItem *list_item = user_data;

  GObject *      obj = gtk_list_item_get_item (list_item);
  GdnThreadInfo *info = GDN_THREAD_INFO (obj);
  SCM            bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
#endif
}

static void
name_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *button;
  GtkLabel * label;

  button = GTK_BUTTON (gtk_button_new_with_label (NULL));
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_xalign (label, 0);
  gtk_label_set_width_chars (label, 30);
  gtk_list_item_set_child (list_item, GTK_WIDGET (button));

  g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (name_activate),
                    list_item);
}

static void
name_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *    button;
  GtkLabel *     label;
  GdnThreadInfo *info;
  GObject *      obj;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = GTK_LABEL (gtk_button_get_child (button));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_THREAD_INFO (obj);
  gtk_label_set_text (label, gdn_thread_info_get_name (info));
  if (label)
    gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);
}

static void
name_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkButton *button;
  GtkLabel * label;

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = GTK_LABEL (gtk_button_get_child (button));
  gtk_label_set_text (label, "");
}

static void
emoji_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
emoji_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *     label;
  GObject *      obj;
  GdnThreadInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_THREAD_INFO (obj);
  const char *name = gdn_thread_info_get_name (info);
  char *      emoji = gdn_string_hash_to_emoji (name);
  gtk_label_set_text (label, emoji);
  free (emoji);
}

static void
emoji_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
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
  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
active_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *     label;
  GObject *      obj;
  GdnThreadInfo *info;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_THREAD_INFO (obj);
  if (gdn_thread_info_get_active (info))
    gtk_label_set_text (label, "Y");
  else
    gtk_label_set_text (label, "N");
}

static void
active_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
               GtkListItem *                     list_item,
               G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

/* This GSourceFunc is used on-idle to move the thread temp_store to
 * the thread GListStore that the GUI uses. This has to occur in the
 * GTK thread. */
static gboolean
update_store (gpointer user_data)
{
  GdnThreadView * self = GDN_THREAD_VIEW (user_data);
  gsize           len;
  GdnThreadInfo **array;

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
// Guile API
////////////////////////////////////////////////////////////////

SCM
gdn_thread_view_to_scm (GdnThreadView *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_THREAD_VIEW);
  return scm_make_foreign_object_1 (scm_thread_view_type, self);
}

static SCM
scm_update_threads_x (SCM s_self)
{
  scm_assert_foreign_object_type (scm_thread_view_type, s_self);
  GdnThreadView *self = scm_foreign_object_ref (s_self, 0);

  /* Since this is Guile API, it is probably being called from a Guile
   * thread instead of the Gtk thread.  GListModel's can only be
   * updated from the Gtk thread.  So we stage the changes to the temp
   * storage array, and then update the GListModel in the next
   * available tick. */
  g_mutex_lock (&lock);
  SCM    all_threads = scm_vector (scm_all_threads ());
  size_t len = scm_c_vector_length (all_threads);
  g_ptr_array_set_size (self->temp_store, 0);
  for (size_t i = 0; i < len; i++)
    {
      SCM            thrd;
      GdnThreadInfo *info;

      thrd = scm_c_vector_ref (all_threads, i);
      info = gdn_thread_info_new_from_scm (thrd);
      g_ptr_array_insert (self->temp_store, -1, info);
    }
  g_mutex_unlock (&lock);
  g_idle_add (update_store, self);
  return SCM_UNSPECIFIED;
}

void
gdn_thread_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-thread-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_thread_view_type = scm_make_foreign_object_type (name, slots, NULL);

  scm_c_define_gsubr ("gdn-update-threads!", 1, 0, 0, scm_update_threads_x);
  scm_c_export ("gdn-update-threads!", NULL);
}
