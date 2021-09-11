/* guidance-module-view.c
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

/* NOTES:
 * so it is an expanding list with 3 levels
 * The first level are the category: library, site-dir, path, etc)
 * The 2nd level is the module reference "(ice-9 boot-9)" etc
 * The 3rd level are the procedures that module-for-each  returns
 *   For 3rd level, there are "add trap buttons"

 * The top layer of the list model is fixed
 * The 2nd layer of the list model is generated dynamically in response
 * to the module load hook.
 * The 3rd layer is generated and re-generated dynamically at expansion time.
 */

#include "guidance-module-view.h"
#include "guidance-emoji.h"
#include "guidance-module-info.h"
#include <glib-unix.h>

/* An expandable list view.
 - 1st level is just a label
 - 2nd level is also a label
 - 3rd level is a label with a source button and a trap button
   - The source button jumps to the function in the source view
   - The trap button pop a yes/no dialog then sets a trap a that procedure
*/

struct _GdnModuleView
{
  GtkBox parent_instance;

  GtkScrolledWindow *scrolled_window;
  GtkListView *      column_view;

  GListStore *model;
};

G_DEFINE_TYPE (GdnModuleView, gdn_module_view, GTK_TYPE_BOX)

static GdnModuleView *_self;

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
typedef void (*factory_func_t) (GtkSignalListItemFactory *self,
                                GtkListItem *             listitem,
                                gpointer                  user_data);

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

static void
module_activate (GtkListView *list, guint position, gpointer unused);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_module_view_class_init (GdnModuleViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/module-view.ui");

#define BIND(x)                                                                \
  gtk_widget_class_bind_template_child (widget_class, GdnModuleView, x)

  BIND (column_view);
  BIND (name_col);
  BIND (emoji_col);
  BIND (active_col);

#undef BIND
}

static void
gdn_module_view_init (GdnModuleView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));

  _self = self;
  self->model = g_list_store_new (GDN_MODULE_INFO_TYPE);

  set_column_view_model (self->column_view, G_LIST_MODEL (self->model));
  add_column_factory (self->name_col, name_setup, name_bind, name_unbind);
  add_column_factory (self->emoji_col, emoji_setup, emoji_bind, emoji_unbind);
  add_column_factory (self->active_col, active_setup, active_bind,
                      active_unbind);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static void
name_activate (GtkButton *self, gpointer user_data)
{
#if 1
  g_debug ("activate module");
#else
  GtkListItem *list_item = user_data;

  GObject *      obj = gtk_list_item_get_item (list_item);
  GdnModuleInfo *info = GDN_MODULE_INFO (obj);
  SCM            bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
#endif
}

static void
name_setup (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkExpander *expander;
  GtkLabel *   label;

  expander = gtk_tree_expander_new ();
  gtk_list_item_set_child (list_item, expander);

  label = gtk_label_new (NULL);
  gtk_widget_set_margin_start (label, 5);
  gtk_widget_set_margin_end (label, 5);
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), label);
}

static void
name_bind (GtkListItemFactory *factory,
           GtkListItem *       list_item,
           gpointer            user_data)
{
  GtkButton *    button;
  GtkLabel *     label;
  GdnModuleInfo *info;
  GObject *      obj;

  static void GtkTreeListRow *list_row;
  GtkWidget *                 expander;
  GtkWidget *                 label;
  gpointer                    item;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);

  expander = gtk_list_item_get_child (list_item);
  gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (expander), list_row);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));

  gtk_label_set_label (GTK_LABEL (label), gdn_environment_info_get_key (item));

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = gtk_button_get_child (button);
  obj = gtk_list_item_get_item (list_item);
  info = GDN_MODULE_INFO (obj);
  gtk_label_set_text (label, gdn_module_info_get_name (info));
  if (label)
    gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);
}

static void
name_unbind (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkButton *button;
  GtkLabel * label;
  GObject *  obj;

  GtkTreeListRow *list_row;
  GtkWidget *     expander;
  GtkWidget *     label;

  list_row = gtk_list_item_get_item (list_item);
  expander = gtk_list_item_get_child (list_item);
  label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (expander));
  gtk_label_set_label (GTK_LABEL (label), "");

  button = GTK_BUTTON (gtk_list_item_get_child (list_item));
  label = gtk_button_get_child (button);
  gtk_label_set_text (label, "");
}

static void
emoji_setup (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_list_item_set_child (list_item, label);
}

static void
emoji_bind (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkLabel *     label;
  GObject *      obj;
  GdnModuleInfo *info;
  char *         location;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_MODULE_INFO (obj);
  const char *name = gdn_module_info_get_name (info);
  char *      emoji = gdn_string_hash_to_emoji (name);
  gtk_label_set_text (label, emoji);
  free (emoji);
}

static void
emoji_unbind (GtkListItemFactory *factory,
              GtkListItem *       list_item,
              gpointer            user_data)
{
  GtkLabel *label;
  GObject * obj;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  gtk_label_set_text (label, "");
}

static void
active_setup (GtkListItemFactory *factory,
              GtkListItem *       list_item,
              gpointer            user_data)
{
  GtkLabel *label;

  label = gtk_label_new (NULL);
  gtk_list_item_set_child (list_item, label);
}

static void
active_bind (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkLabel *     label;
  GObject *      obj;
  GdnModuleInfo *info;
  char *         location;

  label = GTK_LABEL (gtk_list_item_get_child (list_item));
  obj = gtk_list_item_get_item (list_item);
  info = GDN_MODULE_INFO (obj);
  if (gdn_module_info_get_active (info))
    gtk_label_set_text (label, "Y");
  else
    gtk_label_set_text (label, "N");
}

static void
active_unbind (GtkListItemFactory *factory,
               GtkListItem *       list_item,
               gpointer            user_data)
{
  GtkLabel *label;
  GObject * obj;

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

static SCM
scm_update_modules (void)
{
  gdn_module_info_store_update (_self->model);
}

void
gdn_module_view_guile_init (void)
{
  scm_c_define_gsubr ("gdn-update-modules", 0, 0, 0, scm_update_modules);
}
