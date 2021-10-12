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

#include "guidance-module-view.h"
#include "guidance-module-info.h"
#include <glib-unix.h>


struct _GdnModuleView
{
  GtkBox parent_instance;

  GtkScrolledWindow *scrolled_window;
  GtkListView *      list_view;

  GListStore *      store;
  GtkTreeListModel *model;
};

G_DEFINE_TYPE (GdnModuleView, gdn_module_view, GTK_TYPE_BOX)

enum
{
  TRAP = 0,
  LOCATION,
  N_SIGNALS
};

typedef struct _GdnModuleViewAndListItem
{
  GdnModuleView *view;
  GtkListItem *  item;
} GdnModuleViewAndListItem;


////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static unsigned signals[N_SIGNALS];

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
static void mvli_free (GdnModuleViewAndListItem *mvli, GClosure *closure);

static int module_info_compare (const void *a, const void *b, void *user_data);

static SCM scm_module_view_type;
static SCM add_binding_key_value_proc;
static SCM module_name_proc;
static SCM module_filename_proc;
static SCM module_obarray_proc;

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

  signals[TRAP] =
      g_signal_new ("trap", G_TYPE_FROM_CLASS (klass),
                    G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, 0, NULL, NULL,
                    NULL, G_TYPE_NONE, 1, G_TYPE_UINT64);
  signals[LOCATION] = g_signal_new ("location", G_TYPE_FROM_CLASS (klass),
                                    G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE, 0,
                                    NULL, NULL, NULL, G_TYPE_NONE, 3,
                                    G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT);
}

static void
gdn_module_view_init (GdnModuleView *self)
{
  GtkListItemFactory *factory;

  gtk_widget_init_template (GTK_WIDGET (self));

  self->store = g_list_store_new (GDN_MODULE_INFO_TYPE);
  self->model = gtk_tree_list_model_new (
      G_LIST_MODEL (self->store), FALSE, FALSE,
      (GtkTreeListModelCreateModelFunc) gdn_module_info_get_child_model, NULL,
      NULL);

  GtkNoSelection *nosel_model =
      gtk_no_selection_new (G_LIST_MODEL (self->model));
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
  g_assert (self != NULL);

  GdnModuleViewAndListItem *mvli = (GdnModuleViewAndListItem *) user_data;
  GdnModuleInfo *           info = GDN_MODULE_INFO (mvli->item);
  char *         path = gdn_module_info_get_abs_path (info);
  g_signal_emit (mvli->view, signals[LOCATION], 0, g_strdup (path), 0, 0);
  free (path);
}

static void
procedure_open_activate (GtkButton *self, gpointer user_data)
{
  g_assert (self != NULL);
  g_assert (user_data != NULL);

  g_debug ("activate procedure open");
}

static void
procedure_trap_activate (GtkButton *self, gpointer user_data)
{
  g_assert (self != NULL);
  g_assert (user_data != NULL);

  g_debug ("activate procedure trap");
  GdnModuleInfo *info = GDN_MODULE_INFO (user_data);

  // FIXME: add a confirmation dialog
  SCM proc = gdn_module_info_get_procedure (info);
  if (scm_is_true (scm_procedure_p (proc)))
    g_signal_emit (self, signals[TRAP], 0, SCM_UNPACK (proc));
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

  g_assert (factory != NULL);
  g_assert (user_data == NULL);

  expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
  gtk_list_item_set_child (list_item, GTK_WIDGET (expander));

  box = GTK_BOX (gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10));
  gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), GTK_WIDGET (box));

  label = GTK_LABEL (gtk_label_new (NULL));
  gtk_widget_set_margin_start (GTK_WIDGET (label), 5);
  gtk_widget_set_margin_end (GTK_WIDGET (label), 5);
  gtk_widget_set_size_request (GTK_WIDGET (label), 173, -1);
  gtk_label_set_xalign (label, 0.0);
  gtk_box_append (box, GTK_WIDGET (label));

  button = GTK_BUTTON (gtk_button_new_from_icon_name ("document-open"));
  gtk_box_append (box, GTK_WIDGET (button));

  button = GTK_BUTTON (gtk_button_new_from_icon_name ("process-stop"));
  gtk_box_append (box, GTK_WIDGET (button));
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

  g_assert (factory != NULL);
  GdnModuleView *self = user_data;

  list_row = gtk_list_item_get_item (list_item);
  item = gtk_tree_list_row_get_item (list_row);
  info = GDN_MODULE_INFO (item);

  expander = GTK_TREE_EXPANDER (gtk_list_item_get_child (list_item));
  gtk_tree_expander_set_list_row (expander, list_row);
  box = GTK_BOX (gtk_tree_expander_get_child (expander));
  label = GTK_LABEL (gtk_widget_get_first_child (GTK_WIDGET (box)));
  open_button = GTK_BUTTON (gtk_widget_get_next_sibling (GTK_WIDGET (label)));
  gtk_button_set_has_frame (open_button, FALSE);
  trap_button =
      GTK_BUTTON (gtk_widget_get_next_sibling (GTK_WIDGET (open_button)));
  gtk_button_set_has_frame (trap_button, FALSE);

  printf ("BLAMMO %s\n", gdn_module_info_get_name (info));
  gtk_label_set_label (GTK_LABEL (label), gdn_module_info_get_name (info));
  gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);

  if (gdn_module_info_is_module (info))
    {
      gtk_widget_show (GTK_WIDGET (open_button));
      gtk_widget_hide (GTK_WIDGET (trap_button));
      GdnModuleViewAndListItem *mvli = g_new0 (GdnModuleViewAndListItem, 1);
      mvli->view = g_object_ref (self);
      mvli->item = g_object_ref (list_item);
      g_signal_connect_data (open_button, "clicked",
                             G_CALLBACK (module_open_activate), mvli,
                             (GClosureNotify) mvli_free, G_CONNECT_AFTER);
    }
  else if (gdn_module_info_is_procedure (info))
    {
      gtk_widget_show (GTK_WIDGET (open_button));
      gtk_widget_show (GTK_WIDGET (trap_button));
      g_signal_connect (open_button, "clicked",
                        G_CALLBACK (procedure_open_activate), info);
      g_signal_connect (trap_button, "clicked",
                        G_CALLBACK (procedure_trap_activate), info);
    }
  else
    g_return_if_reached ();
}

static void
entry_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
              GtkListItem *                     list_item,
              G_GNUC_UNUSED gpointer            user_data)
{
  g_assert (factory != NULL);
  g_assert (list_item != NULL);
  g_assert (user_data == NULL);
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

static void
mvli_free (GdnModuleViewAndListItem *mvli, G_GNUC_UNUSED GClosure *closure)
{
  g_object_unref (mvli->view);
  mvli->view = NULL;
  g_object_unref (mvli->item);
  mvli->item = NULL;
  free (mvli);
}

/* A GCompareDataFunc that comparestwo GdnModuleInfo by their name. */
static int
module_info_compare (const void *a, const void *b, void *user_data)
{
  g_assert (user_data == NULL);
  GdnModuleInfo *entry_a = GDN_MODULE_INFO ((gpointer) a);
  GdnModuleInfo *entry_b = GDN_MODULE_INFO ((gpointer) b);
  return strcmp (gdn_module_info_get_name (entry_a),
                 gdn_module_info_get_name (entry_b));
}

static int
module_info_eq (const void *a, const void *b)
{
  GdnModuleInfo *entry_a = GDN_MODULE_INFO ((gpointer) a);
  GdnModuleInfo *entry_b = GDN_MODULE_INFO ((gpointer) b);
  return strcmp (gdn_module_info_get_name (entry_a),
                 gdn_module_info_get_name (entry_b)) == 0;
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////
static GListStore *_inner_store = NULL;

/* This procedure is an internal procedure used when creating a
 * GListStore of top-level procedures from an SCM module. This
 * unfortunate proc is a work-around because of having to use
 * scm_hash_for_each. */
static SCM
scm_add_binding_key_value (SCM key, SCM value)
{
  GdnModuleInfo *info;
  char *         str;

  info = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  gdn_module_info_set_module (info, value);
  str = scm_to_utf8_string (scm_symbol_to_string (key));
  gdn_module_info_set_name (info, str);
  free (str);

  g_list_store_insert_sorted (_inner_store, info, module_info_compare, NULL);

  return SCM_UNSPECIFIED;
}

static SCM
scm_module_name (SCM module)
{
  return scm_call_1 (module_name_proc, module);
}

/* This Guile procedure, given a Guile #<directory> module entry,
 * updates the module info store. If an existing entry by this name
 * exists, it is replaced. Returns #t if store is modified. */
static SCM
scm_add_module_x (SCM s_self, SCM module)
{
  scm_assert_foreign_object_type (scm_module_view_type, s_self);
  GdnModuleView *self = scm_foreign_object_ref (s_self, 0);

  SCM            symlist;
  SCM            name;
  char *         str;
  GdnModuleInfo *entry, *found;
  guint          position;
  SCM            ret = SCM_BOOL_F;

  symlist = scm_module_name (module);
  name = scm_object_to_string (symlist, SCM_UNDEFINED);
  str = scm_to_utf8_string (name);
  entry = g_object_new (GDN_MODULE_INFO_TYPE, NULL);
  gdn_module_info_set_name (entry, str);
  free (str);
  gdn_module_info_set_module (entry, module);

  if (g_list_store_find_with_equal_func (self->store, entry, module_info_eq,
                                         &position))
    {
      found = g_list_model_get_item (G_LIST_MODEL (self->store), position);
      if (!scm_is_eq (gdn_module_info_get_module (found),
                      gdn_module_info_get_module (entry)))
        {
          gdn_module_info_set_module (found,
                                      gdn_module_info_get_module (entry));
          g_list_model_items_changed (G_LIST_MODEL (self->store), position, 1,
                                      1);
          ret = SCM_BOOL_T;
        }
      g_object_unref (entry);
    }
  else
    {
      g_list_store_insert_sorted (self->store, entry, module_info_compare,
                                  NULL);
      ret = SCM_BOOL_T;
    }

  return ret;
}

/* This Guile procedure deletes all the entries in the module info
 * _STORE.  The return value is unspecified. */
static SCM
scm_clear_modules_x (SCM s_self)
{
  scm_assert_foreign_object_type (scm_module_view_type, s_self);
  GdnModuleView *self = scm_foreign_object_ref (s_self, 0);

  g_list_store_remove_all (self->store);
  return SCM_UNSPECIFIED;
}

/* This procedure initializes the Guile API for the Module page of the
   GUI. */
void
gdn_module_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-module-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_module_view_type = scm_make_foreign_object_type (name, slots, NULL);

  /* Guile functions used in this module. */
  add_binding_key_value_proc =
      scm_c_define_gsubr ("%gdn-add-binding-key-value", 2, 0, 0,
                          (scm_t_subr) scm_add_binding_key_value);
  module_filename_proc = scm_variable_ref (scm_c_lookup ("module-filename"));
  module_name_proc = scm_variable_ref (scm_c_lookup ("module-name"));
  module_obarray_proc = scm_variable_ref (scm_c_lookup ("module-obarray"));

  /* The public API */
  scm_c_define_gsubr ("gdn-add-module!", 2, 0, 0,
                      (scm_t_subr) scm_add_module_x);
  scm_c_define_gsubr ("gdn-clear-modules!", 1, 0, 0,
                      (scm_t_subr) scm_clear_modules_x);
}
