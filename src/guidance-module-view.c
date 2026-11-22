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
#include "guidance-xgtk.h"

struct _GdnModuleView
{
  GtkBox parent_instance;

  GtkScrolledWindow *scrolled_window;
  GtkListView *      list_view;

  GListStore *      store; /* of GdnModuleInfo for modules */
  GtkTreeListModel *model;
};

G_DEFINE_TYPE (GdnModuleView, gdn_module_view, GTK_TYPE_BOX)

enum
{
  TRAP = 0,
  LOCATION,
  EXPAND,
  N_SIGNALS
};

static unsigned signals[N_SIGNALS];
static SCM      scm_module_view_type;

static GListModel *create_child_model (gpointer item, gpointer user_data);
static void        entry_setup (GtkSignalListItemFactory *factory,
                                GtkListItem *             list_item,
                                gpointer                  user_data);
static void        entry_bind (GtkSignalListItemFactory *factory,
                               GtkListItem *             list_item,
                               gpointer                  user_data);
static void        entry_unbind (GtkSignalListItemFactory *factory,
                                 GtkListItem *             list_item,
                                 gpointer                  user_data);

////////////////////////////////////////////////////////////////
// INITIALIZATION
////////////////////////////////////////////////////////////////

static void
gdn_module_view_dispose (GObject *object)
{
  GdnModuleView *self = GDN_MODULE_VIEW (object);
  g_clear_object (&self->model);
  g_clear_object (&self->store);
  G_OBJECT_CLASS (gdn_module_view_parent_class)->dispose (object);
}

static void
gdn_module_view_class_init (GdnModuleViewClass *klass)
{
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
  G_OBJECT_CLASS (klass)->dispose = gdn_module_view_dispose;

  gtk_widget_class_set_template_from_resource (
      widget_class, "/com/lonelycactus/Guidance/gtk/module-view.ui");
  gtk_widget_class_bind_template_child (widget_class, GdnModuleView,
                                        scrolled_window);
  gtk_widget_class_bind_template_child (widget_class, GdnModuleView,
                                        list_view);

  signals[TRAP] = g_signal_new ("trap", G_TYPE_FROM_CLASS (klass),
                                G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                                G_TYPE_NONE, 2, G_TYPE_STRING, G_TYPE_STRING);
  signals[LOCATION] = g_signal_new ("location", G_TYPE_FROM_CLASS (klass),
                                    G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                                    G_TYPE_NONE, 3, G_TYPE_STRING, G_TYPE_INT,
                                    G_TYPE_INT);
  signals[EXPAND] = g_signal_new ("expand", G_TYPE_FROM_CLASS (klass),
                                  G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
                                  G_TYPE_NONE, 1, G_TYPE_STRING);
}

static void
gdn_module_view_init (GdnModuleView *self)
{
  GtkListItemFactory *factory;

  gtk_widget_init_template (GTK_WIDGET (self));

  self->store = g_list_store_new (GDN_MODULE_INFO_TYPE);
  self->model = gtk_tree_list_model_new (
      G_LIST_MODEL (g_object_ref (self->store)), FALSE, FALSE,
      create_child_model, self, NULL);

  gtk_list_view_set_model (
      self->list_view,
      GTK_SELECTION_MODEL (gtk_no_selection_new (
          G_LIST_MODEL (g_object_ref (self->model)))));

  factory = gtk_signal_list_item_factory_new ();
  g_signal_connect (factory, "setup", G_CALLBACK (entry_setup), self);
  g_signal_connect (factory, "bind", G_CALLBACK (entry_bind), self);
  g_signal_connect (factory, "unbind", G_CALLBACK (entry_unbind), self);
  gtk_list_view_set_factory (self->list_view, factory);
  g_object_unref (factory);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

/* A GtkTreeListModelCreateModelFunc.  Modules have their procedures as
 * children.  GTK calls this as soon as a row is shown, to learn whether
 * it can be expanded, so the procedures are not requested here but
 * when the row is expanded; see row_expanded. */
static GListModel *
create_child_model (gpointer item, G_GNUC_UNUSED gpointer user_data)
{
  GdnModuleInfo *info = GDN_MODULE_INFO (item);

  if (!gdn_module_info_is_module (info))
    return NULL;
  return G_LIST_MODEL (g_object_ref (gdn_module_info_get_children (info)));
}

/* Requests a module's procedures each time its row is expanded, so
 * that new definitions appear. */
static void
row_expanded (GtkTreeListRow *row, G_GNUC_UNUSED GParamSpec *pspec,
              gpointer user_data)
{
  GdnModuleInfo *info;

  if (!gtk_tree_list_row_get_expanded (row))
    return;
  info = GDN_MODULE_INFO (gtk_tree_list_row_get_item (row));
  if (gdn_module_info_is_module (info))
    g_signal_emit (user_data, signals[EXPAND], 0,
                   gdn_module_info_get_module_name (info));
  g_object_unref (info);
}

static GdnModuleInfo *
list_item_get_info (GtkListItem *list_item)
{
  GtkTreeListRow *row = gtk_list_item_get_item (list_item);
  if (row == NULL)
    return NULL;
  return GDN_MODULE_INFO (gtk_tree_list_row_get_item (row));
}

static void
open_clicked (GtkButton *button, gpointer user_data)
{
  GtkListItem *  list_item = user_data;
  GdnModuleView *self =
      GDN_MODULE_VIEW (g_object_get_data (G_OBJECT (button), "view"));
  GdnModuleInfo *info = list_item_get_info (list_item);

  if (info != NULL && gdn_module_info_get_file (info) != NULL)
    g_signal_emit (self, signals[LOCATION], 0, gdn_module_info_get_file (info),
                   MAX (gdn_module_info_get_line (info), 1), 0);
  g_clear_object (&info);
}

static void
trap_clicked (GtkButton *button, gpointer user_data)
{
  GtkListItem *  list_item = user_data;
  GdnModuleView *self =
      GDN_MODULE_VIEW (g_object_get_data (G_OBJECT (button), "view"));
  GdnModuleInfo *info = list_item_get_info (list_item);

  if (info != NULL && gdn_module_info_get_trappable (info))
    g_signal_emit (self, signals[TRAP], 0,
                   gdn_module_info_get_module_name (info),
                   gdn_module_info_get_name (info));
  g_clear_object (&info);
}

static void
entry_setup (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
             GtkListItem *                           list_item,
             gpointer                                user_data)
{
  GtkWidget *expander = gtk_tree_expander_new ();
  GtkWidget *box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
  GtkWidget *label = gtk_label_new (NULL);
  GtkWidget *open_button = gtk_button_new_from_icon_name ("document-open");
  GtkWidget *trap_button = gtk_button_new_from_icon_name ("media-record");

  gtk_label_set_xalign (GTK_LABEL (label), 0.0);
  gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
  gtk_widget_set_hexpand (label, TRUE);
  gtk_widget_set_size_request (label, 200, -1);
  gtk_box_append (GTK_BOX (box), label);

  gtk_button_set_has_frame (GTK_BUTTON (open_button), FALSE);
  gtk_widget_set_tooltip_text (open_button, "Show the source");
  g_object_set_data (G_OBJECT (open_button), "view", user_data);
  g_signal_connect (open_button, "clicked", G_CALLBACK (open_clicked),
                    list_item);
  gtk_box_append (GTK_BOX (box), open_button);

  gtk_button_set_has_frame (GTK_BUTTON (trap_button), FALSE);
  gtk_widget_set_tooltip_text (trap_button, "Pause when this is called");
  g_object_set_data (G_OBJECT (trap_button), "view", user_data);
  g_signal_connect (trap_button, "clicked", G_CALLBACK (trap_clicked),
                    list_item);
  gtk_box_append (GTK_BOX (box), trap_button);

  gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), box);
  gtk_list_item_set_child (list_item, expander);
}

static void
entry_unbind (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
              GtkListItem *                           list_item,
              G_GNUC_UNUSED gpointer                  user_data)
{
  GtkTreeListRow *row = gtk_list_item_get_item (list_item);
  gulong          id = GPOINTER_TO_SIZE (
      g_object_get_data (G_OBJECT (list_item), "expanded-handler"));

  if (id != 0 && row != NULL)
    g_signal_handler_disconnect (row, id);
  g_object_set_data (G_OBJECT (list_item), "expanded-handler", NULL);
}

static void
entry_bind (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
            GtkListItem *                           list_item,
            gpointer                                user_data)
{
  GtkTreeListRow * row = gtk_list_item_get_item (list_item);
  GtkTreeExpander *expander =
      GTK_TREE_EXPANDER (gtk_list_item_get_child (list_item));
  GtkWidget *    box = gtk_tree_expander_get_child (expander);
  GtkWidget *    label = gtk_widget_get_first_child (box);
  GtkWidget *    open_button = gtk_widget_get_next_sibling (label);
  GtkWidget *    trap_button = gtk_widget_get_next_sibling (open_button);
  GdnModuleInfo *info = list_item_get_info (list_item);

  gtk_tree_expander_set_list_row (expander, row);
  if (gdn_module_info_is_module (info))
    {
      gulong id = g_signal_connect (row, "notify::expanded",
                                    G_CALLBACK (row_expanded), user_data);
      g_object_set_data (G_OBJECT (list_item), "expanded-handler",
                         GSIZE_TO_POINTER (id));
    }
  gtk_label_set_text (GTK_LABEL (label), gdn_module_info_get_name (info));
  gtk_widget_set_visible (open_button,
                          gdn_module_info_get_file (info) != NULL);
  gtk_widget_set_visible (trap_button, gdn_module_info_get_trappable (info));
  g_object_unref (info);
}

////////////////////////////////////////////////////////////////
// GUILE API
////////////////////////////////////////////////////////////////

SCM
gdn_module_view_to_scm (GdnModuleView *self)
{
  g_assert (GDN_IS_MODULE_VIEW (self));
  return scm_make_foreign_object_1 (scm_module_view_type, self);
}

static char *
vector_string_ref (SCM vec, size_t i)
{
  return scm_to_utf8_string (scm_c_vector_ref (vec, i));
}

/* GUILE THREAD: (gdn-update-modules! view modules), where MODULES is a
 * vector of #(NAME FILE).  Replaces the module list. */
static SCM
scm_update_modules_x (SCM s_self, SCM modules)
{
  scm_assert_foreign_object_type (scm_module_view_type, s_self);
  SCM_ASSERT_TYPE (scm_is_vector (modules), modules, SCM_ARG2,
                   "gdn-update-modules!", "vector");

  GdnModuleView *self = scm_foreign_object_ref (s_self, 0);
  size_t         len = scm_c_vector_length (modules);
  GPtrArray *    items = g_ptr_array_new_full (len, g_object_unref);

  for (size_t i = 0; i < len; i++)
    {
      SCM   entry = scm_c_vector_ref (modules, i);
      char *name = vector_string_ref (entry, 0);
      char *file = vector_string_ref (entry, 1);
      g_ptr_array_add (items, gdn_module_info_new_module (name, file));
      free (name);
      free (file);
    }
  xgtk_list_store_replace_all_on_main (self->store, items);
  return SCM_UNSPECIFIED;
}

typedef struct
{
  GdnModuleView *view;
  char *         module_name;
  GPtrArray *    procedures; /* of GdnModuleInfo */
} ProcedureUpdate;

static void
procedure_update_free (gpointer data)
{
  ProcedureUpdate *u = data;
  g_object_unref (u->view);
  g_free (u->module_name);
  g_ptr_array_unref (u->procedures);
  g_free (u);
}

/* GTK THREAD: Fills in a module's procedures. */
static gboolean
apply_procedure_update (gpointer data)
{
  ProcedureUpdate *u = data;
  GListModel *     modules = G_LIST_MODEL (u->view->store);
  guint            n = g_list_model_get_n_items (modules);

  for (guint i = 0; i < n; i++)
    {
      GdnModuleInfo *info = g_list_model_get_item (modules, i);
      if (g_strcmp0 (gdn_module_info_get_module_name (info), u->module_name)
          == 0)
        {
          GListStore *children = gdn_module_info_get_children (info);
          g_list_store_splice (
              children, 0, g_list_model_get_n_items (G_LIST_MODEL (children)),
              u->procedures->pdata, u->procedures->len);
          g_object_unref (info);
          break;
        }
      g_object_unref (info);
    }
  return G_SOURCE_REMOVE;
}

/* GUILE THREAD: (gdn-set-module-procedures! view module-name procs),
 * where PROCS is a vector of #(NAME FILE LINE TRAPPABLE?). */
static SCM
scm_set_module_procedures_x (SCM s_self, SCM s_module_name, SCM procs)
{
  scm_assert_foreign_object_type (scm_module_view_type, s_self);
  SCM_ASSERT_TYPE (scm_is_string (s_module_name), s_module_name, SCM_ARG2,
                   "gdn-set-module-procedures!", "string");
  SCM_ASSERT_TYPE (scm_is_vector (procs), procs, SCM_ARG3,
                   "gdn-set-module-procedures!", "vector");

  ProcedureUpdate *u = g_new0 (ProcedureUpdate, 1);
  char *           module_name = scm_to_utf8_string (s_module_name);
  size_t           len = scm_c_vector_length (procs);

  u->view = g_object_ref (scm_foreign_object_ref (s_self, 0));
  u->module_name = g_strdup (module_name);
  u->procedures = g_ptr_array_new_full (len, g_object_unref);
  for (size_t i = 0; i < len; i++)
    {
      SCM   entry = scm_c_vector_ref (procs, i);
      char *name = vector_string_ref (entry, 0);
      char *file = vector_string_ref (entry, 1);
      g_ptr_array_add (u->procedures,
                       gdn_module_info_new_procedure (
                           module_name, name, file,
                           scm_to_int (scm_c_vector_ref (entry, 2)),
                           scm_is_true (scm_c_vector_ref (entry, 3))));
      free (name);
      free (file);
    }
  free (module_name);
  xgtk_invoke_on_main (apply_procedure_update, u, procedure_update_free);
  return SCM_UNSPECIFIED;
}

void
gdn_module_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-module-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_module_view_type = scm_make_foreign_object_type (name, slots, NULL);

  scm_c_define_gsubr ("gdn-update-modules!", 2, 0, 0,
                      (scm_t_subr) scm_update_modules_x);
  scm_c_define_gsubr ("gdn-set-module-procedures!", 3, 0, 0,
                      (scm_t_subr) scm_set_module_procedures_x);
  scm_c_export ("gdn-update-modules!", "gdn-set-module-procedures!", NULL);
}
