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
#include "guidance-module-info.h"
#include <glib-unix.h>

/* An expandable list view.
 - 1st level is just a label
 - 2nd level is a label and an open button
 - 3rd level is a label with a source button and a trap button
   - The source button jumps to the function in the source view
   - The trap button pop a yes/no dialog then sets a trap a that procedure
*/

struct _GdnModuleView
{
  GtkBox parent_instance;

  GtkScrolledWindow *scrolled_window;
  GtkListView *      list_view;

  GListStore *model;
};

G_DEFINE_TYPE (GdnModuleView, gdn_module_view, GTK_TYPE_BOX)

static GdnModuleView *_self;

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////
static void entry_setup (GtkListItemFactory *factory,
                         GtkListItem *       list_item,
                         void *              user_data);
static void entry_bind (GtkListItemFactory *factory,
                        GtkListItem *       list_item,
                        void *              user_data);
static void entry_unbind (GtkListItemFactory *factory,
                          GtkListItem *       list_item,
                          void *              user_data);

static void module_open_activate (GtkButton *button, gpointer user_data);
static void procedure_open_activate (GtkButton *button, gpointer user_data);
static void procedure_trap_activate (GtkButton *button, gpointer user_data);

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

  BIND (scrolled_window);
  BIND (list_view);
#undef BIND
}

static void
gdn_module_view_init (GdnModuleView *self)
{
  GtkTreeListModel *  tree_model;
  GtkListItemFactory *factory;

  gtk_widget_init_template (GTK_WIDGET (self));

  tree_model = gdn_module_info_get_tree_model ();
  _self = self;
  self->model = tree_model;
  GtkNoSelection *nosel_model;
  nosel_model = gtk_no_selection_new (
      G_LIST_MODEL (g_object_ref (G_OBJECT (tree_model))));
  gtk_list_view_set_model (self->list_view, GTK_SELECTION_MODEL (nosel_model));

  factory = gtk_signal_list_item_factory_new ();
  g_signal_connect (factory, "setup", G_CALLBACK (entry_setup), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (entry_bind), NULL);
  g_signal_connect (factory, "unbind", G_CALLBACK (entry_unbind), NULL);
  gtk_list_view_set_factory (self->list_view, factory);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static void
module_open_activate (GtkButton *self, gpointer user_data)
{
#if 1
  g_debug ("activate module open");
#else
  GtkListItem *list_item = user_data;

  GObject *      obj = gtk_list_item_get_item (list_item);
  GdnModuleInfo *info = GDN_MODULE_INFO (obj);
  SCM            bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
#endif
}

static void
procedure_open_activate (GtkButton *self, gpointer user_data)
{
#if 1
  g_debug ("activate procedure open");
#else
  GtkListItem *list_item = user_data;

  GObject *      obj = gtk_list_item_get_item (list_item);
  GdnModuleInfo *info = GDN_MODULE_INFO (obj);
  SCM            bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
#endif
}

static void
procedure_trap_activate (GtkButton *self, gpointer user_data)
{
#if 1
  g_debug ("activate procedure trap");
#else
  GtkListItem *list_item = user_data;

  GObject *      obj = gtk_list_item_get_item (list_item);
  GdnModuleInfo *info = GDN_MODULE_INFO (obj);
  SCM            bindings = SCM_PACK (gdn_frame_info_get_bindings (info));
  gdn_binding_info_update_all (bindings);
#endif
}

static void
entry_setup (GtkListItemFactory *factory,
             GtkListItem *       list_item,
             gpointer            user_data)
{
  GtkTreeExpander *expander;
  GtkLabel *   label;
  GtkBox *         box;
  GtkButton *      button;
  GtkImage *       image;

  expander = gtk_tree_expander_new ();
  gtk_list_item_set_child (list_item, expander);

  box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
  gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), box);

  label = gtk_label_new (NULL);
  gtk_widget_set_margin_start (label, 5);
  gtk_widget_set_margin_end (label, 5);
  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_box_append (box, label);

  button = gtk_button_new_from_icon_name ("document-open");
  gtk_box_append (box, button);

  button = gtk_button_new_from_icon_name ("process-stop");
  gtk_box_append (box, button);
}

static void
entry_bind (GtkListItemFactory *factory,
            GtkListItem *       list_item,
            gpointer            user_data)
{
  GtkTreeListRow * list_row;
  GObject *        item;
  GdnModuleInfo *info;
  GtkTreeExpander *expander;
  GtkBox *         box;
  GtkLabel *       label;
  GtkButton *      open_button, *trap_button;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);
  info = GDN_MODULE_INFO (item);

  expander = GTK_TREE_EXPANDER (gtk_list_item_get_child (list_item));
  gtk_tree_expander_set_list_row (expander, list_row);
  box = GTK_BOX (gtk_tree_expander_get_child (expander));
  label = GTK_LABEL (gtk_widget_get_first_child (box));
  open_button = gtk_widget_get_next_sibling (label);
  trap_button = gtk_widget_get_next_sibling (open_button);

  gtk_label_set_label (GTK_LABEL (label), gdn_module_info_get_name (info));
  gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);

  if (gdn_module_info_is_top_level (info))
    {
      gtk_widget_hide (open_button);
      gtk_widget_hide (trap_button);
    }
  else if (gdn_module_info_is_module (info))
    {
      gtk_widget_show (open_button);
      gtk_widget_hide (trap_button);
      g_signal_connect (open_button, "activate", module_open_activate, info);
    }
  else if (gdn_module_info_is_procedure (info))
    {
      gtk_widget_show (open_button);
      gtk_widget_show (trap_button);
      g_signal_connect (open_button, "activate", procedure_open_activate, info);
      g_signal_connect (trap_button, "activate", procedure_trap_activate, info);
    }
}

static void
entry_unbind (GtkListItemFactory *factory,
              GtkListItem *       list_item,
              gpointer            user_data)
{
#if 0
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
#endif
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// Guile API
////////////////////////////////////////////////////////////////

