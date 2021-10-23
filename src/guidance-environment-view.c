/* guidance-environment-view.c
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

#include "guidance-environment-view.h"
#include "guidance-environment-info.h"
#include <glib-unix.h>

struct _GdnEnvironmentView
{
  GtkBox parent_instance;

  GtkColumnView *      column_view;
  GtkColumnViewColumn *key_col;
  GtkColumnViewColumn *value_col;
  GtkTreeListModel *   model;

  SCM                  temp_store;
  GListStore *         store;
};

G_DEFINE_TYPE (GdnEnvironmentView, gdn_environment_view, GTK_TYPE_BOX)

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
static GMutex lock;

static SCM scm_environment_view_type = SCM_BOOL_F;

typedef void (*factory_func_t) (GtkSignalListItemFactory *self,
                                GtkListItem *             listitem,
                                gpointer                  user_data);

static void key_setup (GtkListItemFactory *factory,
                       GtkListItem *       list_item,
                       void *              user_data);
static void
key_bind (GtkListItemFactory *factory, GtkListItem *list_item, void *user_data);
static void        key_unbind (GtkListItemFactory *factory,
                               GtkListItem *       list_item,
                               void *              user_data);
static void        value_setup (GtkListItemFactory *factory,
                                GtkListItem *       list_item,
                                void *              user_data);
static void        value_bind (GtkListItemFactory *factory,
                               GtkListItem *       list_item,
                               void *              user_data);
static void        value_unbind (GtkListItemFactory *factory,
                                 GtkListItem *       list_item,
                                 void *              user_data);
static GListModel *get_child_model (GObject *item, gpointer user_data);
////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_environment_view_class_init (GdnEnvironmentViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/environment-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnEnvironmentView, x)

  BIND (column_view);
  BIND (key_col);
  BIND (value_col);
#undef BIND
}

static void
gdn_environment_view_init (GdnEnvironmentView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  self->temp_store = SCM_BOOL_F;
  self->store = g_list_store_new (GDN_ENVIRONMENT_INFO_TYPE);
  self->model = gtk_tree_list_model_new (
      G_LIST_MODEL (self->store), FALSE, FALSE,
      (GtkTreeListModelCreateModelFunc) get_child_model, NULL, NULL);

  GtkSingleSelection *selection_model;
  selection_model =
      gtk_single_selection_new (G_LIST_MODEL (g_object_ref (self->model)));
  gtk_column_view_set_model (self->column_view,
                             GTK_SELECTION_MODEL (selection_model));

  GtkListItemFactory *key_column_factory;
  key_column_factory = gtk_signal_list_item_factory_new ();
  g_signal_connect (key_column_factory, "setup", G_CALLBACK (key_setup), self);
  g_signal_connect (key_column_factory, "bind", G_CALLBACK (key_bind), self);
  g_signal_connect (key_column_factory, "unbind", G_CALLBACK (key_unbind),
                    self);
  gtk_column_view_column_set_factory (self->key_col, key_column_factory);

  GtkSignalListItemFactory *value_column_factory;
  value_column_factory =
      GTK_SIGNAL_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
  g_signal_connect (value_column_factory, "setup", G_CALLBACK (value_setup),
                    self);
  g_signal_connect (value_column_factory, "bind", G_CALLBACK (value_bind),
                    self);
  g_signal_connect (value_column_factory, "unbind", G_CALLBACK (value_unbind),
                    self);
  gtk_column_view_column_set_factory (
      self->value_col, GTK_LIST_ITEM_FACTORY (value_column_factory));
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static void
key_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           G_GNUC_UNUSED gpointer            user_data)
{
  GtkTreeExpander *expander;
  GtkLabel *   label;

  expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
  label = GTK_LABEL (gtk_label_new (""));
  gtk_widget_set_margin_start (GTK_WIDGET (label), 5);
  gtk_widget_set_margin_end (GTK_WIDGET (label), 5);
  gtk_label_set_xalign (label, 0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (expander));
  gtk_tree_expander_set_child (expander, GTK_WIDGET (label));
}

static void
key_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
          GtkListItem *                     list_item,
          G_GNUC_UNUSED gpointer            user_data)
{
  GtkTreeListRow *list_row;
  GtkWidget *     expander;
  GtkWidget *     label;
  gpointer        item;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);

  expander = gtk_list_item_get_child (list_item);
  gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (expander), list_row);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));

  gtk_label_set_label (GTK_LABEL (label), gdn_environment_info_get_key (item));
}

static void
key_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  g_assert (factory != NULL);

  GtkWidget *expander;
  GtkWidget *label;

  expander = gtk_list_item_get_child (list_item);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));
  gtk_label_set_label (GTK_LABEL (label), "");
}

static void
value_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  g_assert (factory != NULL);

  GtkLabel *label;

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_widget_set_margin_start (GTK_WIDGET (label), 5);
  gtk_widget_set_margin_end (GTK_WIDGET (label), 5);
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
value_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  g_assert (factory != NULL);

  GtkTreeListRow *list_row;
  GtkLabel *      label;
  gpointer        item;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_label (label, gdn_environment_info_get_value (item));
}

static void
value_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  g_assert (factory != NULL);

  GtkWidget *label;

  label = gtk_list_item_get_child (list_item);
  gtk_label_set_label (GTK_LABEL (label), "");
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

/* A GtkTreeListModelCreateModelFunc */
static GListModel *
get_child_model (GObject *item, G_GNUC_UNUSED gpointer user_data)
{
  GdnEnvironmentInfo *info = GDN_ENVIRONMENT_INFO (item);
  GListStore *        store = gdn_environment_info_get_child_model (info);
  return G_LIST_MODEL (store);
}

static gint
compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
  return gdn_environment_info_compare (a, b);
}

/* This GSourceFunc is used on-idle to move the SCM temp_store to the
 * environment GListStore that the GUI uses. This has to occur in the
 * GTK thread. */
static gboolean
update_store (gpointer user_data)
{
  GdnEnvironmentView *self = GDN_ENVIRONMENT_VIEW (user_data);

  g_mutex_lock (&lock);
  SCM entries = self->temp_store;
  for (size_t i = 0; i < scm_c_vector_length (entries); i++)
    {
      SCM                 entry;
      SCM                 key, value, children;
      GdnEnvironmentInfo *info;
      gboolean            found;
      guint               position;

      entry = scm_c_vector_ref (entries, i);

      key = scm_c_vector_ref (entry, 0);
      value = scm_c_vector_ref (entry, 1);
      children = scm_c_vector_ref (entry, 2);

      info = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
      info->key = scm_to_utf8_string (key);
      info->value = scm_to_utf8_string (value);
      info->list = NULL;

      found = g_list_store_find_with_equal_func (
          self->store, info, gdn_environment_info_equal, &position);
      if (found)
        {
          g_object_unref (info);
          info = g_list_model_get_object (self->store, position);
        }
      if (scm_is_vector (children))
        {
          for (size_t j = 0; j < scm_c_vector_length (children); j++)
            {
              SCM                 child;
              GdnEnvironmentInfo *kiddo;

              child = scm_c_vector_ref (children, j);
              kiddo = g_object_new (GDN_ENVIRONMENT_INFO_TYPE, NULL);
              kiddo->key = scm_to_utf8_string (scm_c_vector_ref (child, 0));
              kiddo->value = scm_to_utf8_string (scm_c_vector_ref (child, 1));
              kiddo->list = NULL;
              GSList *entry2 = g_slist_find_custom (
                  info->list, kiddo, gdn_environment_info_compare);
              if (entry2)
                {
                  /* Overwrite an existing entry */
                  GdnEnvironmentInfo *info2 =
                      (GdnEnvironmentInfo *) (entry2->data);
                  free (info2->value);
                  info2->value = g_strdup (kiddo->value);
                  g_object_unref (kiddo);
                }
              else
                {
                  info->list = g_slist_insert_sorted_with_data (
                      info->list, kiddo, compare, NULL);
                }
            }
        }
      if (!found)
        {
          position =
              g_list_store_insert_sorted (self->store, info, compare, NULL);
        }
    }
  g_mutex_unlock (&lock);

  /* We only run this once. */
  return G_SOURCE_REMOVE;
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

SCM
gdn_environment_view_to_scm (GdnEnvironmentView *self)
{
  g_assert_cmpuint (G_OBJECT_TYPE (self), ==, GDN_TYPE_ENVIRONMENT_VIEW);
  return scm_make_foreign_object_1 (scm_environment_view_type, self);
}

static SCM
scm_update_environments_x (SCM s_self, SCM entries)
{
  scm_assert_foreign_object_type (scm_environment_view_type, s_self);
  GdnEnvironmentView *self;
  g_mutex_lock (&lock);
  self = scm_foreign_object_ref (s_self, 0);
  self->temp_store = entries;
  g_mutex_unlock (&lock);
  g_idle_add (update_store, self);
  return SCM_UNSPECIFIED;
}

void
gdn_environment_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-environment-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_environment_view_type = scm_make_foreign_object_type (name, slots, NULL);

  scm_c_define_gsubr ("gdn-update-environments!", 2, 0, 0,
                      (scm_t_subr) scm_update_environments_x);
  scm_c_export ("gdn-update-environments!", NULL);
}
