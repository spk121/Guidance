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
#include "guidance-thread-info.h"
#include "guidance-xgtk.h"

/* The Threads page: the program's main thread, which runs it, and the
 * threads it started.  The REPL makes the rows. */
struct _GdnThreadView
{
  GtkBox parent_instance;

  GtkColumnView *      column_view;
  GtkColumnViewColumn *name_col;
  GtkColumnViewColumn *procedure_col;
  GtkColumnViewColumn *state_col;

  GListStore *store;
};

G_DEFINE_TYPE (GdnThreadView, gdn_thread_view, GTK_TYPE_BOX)

////////////////////////////////////////////////////////////////
// DECLARATIONS
////////////////////////////////////////////////////////////////

static SCM scm_thread_view_type = SCM_BOOL_F;

typedef const char *(*text_getter_t) (GdnThreadInfo *info);

static void add_text_column (GtkColumnViewColumn *col, text_getter_t getter);

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
  BIND (procedure_col);
  BIND (state_col);

#undef BIND
}

static void
gdn_thread_view_init (GdnThreadView *self)
{
  gtk_widget_init_template (GTK_WIDGET (self));
  self->store = g_list_store_new (GDN_THREAD_INFO_TYPE);

  gtk_column_view_set_model (
      self->column_view,
      GTK_SELECTION_MODEL (gtk_no_selection_new (
          G_LIST_MODEL (g_object_ref (self->store)))));
  add_text_column (self->name_col, gdn_thread_info_get_name);
  add_text_column (self->procedure_col, gdn_thread_info_get_procedure);
  add_text_column (self->state_col, gdn_thread_info_get_state);
}

////////////////////////////////////////////////////////////////
// SIGNAL HANDLERS
////////////////////////////////////////////////////////////////

static void
text_setup (G_GNUC_UNUSED GtkListItemFactory *factory,
            GtkListItem *                     list_item,
            G_GNUC_UNUSED gpointer            user_data)
{
  GtkLabel *label = GTK_LABEL (gtk_label_new (NULL));

  gtk_label_set_xalign (label, 0);
  gtk_label_set_ellipsize (label, PANGO_ELLIPSIZE_END);
  gtk_list_item_set_child (list_item, GTK_WIDGET (label));
}

static void
text_bind (G_GNUC_UNUSED GtkListItemFactory *factory,
           GtkListItem *                     list_item,
           gpointer                          user_data)
{
  text_getter_t  getter = (text_getter_t) user_data;
  GtkLabel *     label = GTK_LABEL (gtk_list_item_get_child (list_item));
  GdnThreadInfo *info = GDN_THREAD_INFO (gtk_list_item_get_item (list_item));

  gtk_label_set_text (label, getter (info));
}

static void
text_unbind (G_GNUC_UNUSED GtkListItemFactory *factory,
             GtkListItem *                     list_item,
             G_GNUC_UNUSED gpointer            user_data)
{
  gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)), "");
}

////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////

/* Makes COL show the text that GETTER returns for each row. */
static void
add_text_column (GtkColumnViewColumn *col, text_getter_t getter)
{
  GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();

  g_signal_connect (factory, "setup", G_CALLBACK (text_setup), NULL);
  g_signal_connect (factory, "bind", G_CALLBACK (text_bind), getter);
  g_signal_connect (factory, "unbind", G_CALLBACK (text_unbind), NULL);
  gtk_column_view_column_set_factory (col, factory);
  g_object_unref (factory);
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

/* ROWS is a vector of vectors #(NAME PROCEDURE STATE), made by the
 * REPL. */
static SCM
scm_update_threads_x (SCM s_self, SCM s_rows)
{
  scm_assert_foreign_object_type (scm_thread_view_type, s_self);
  SCM_ASSERT_TYPE (scm_is_vector (s_rows), s_rows, SCM_ARG2,
                   "gdn-update-threads!", "vector");
  GdnThreadView *self = scm_foreign_object_ref (s_self, 0);

  size_t     len = scm_c_vector_length (s_rows);
  GPtrArray *items = g_ptr_array_new_full (len, g_object_unref);
  for (size_t i = 0; i < len; i++)
    {
      GdnThreadInfo *info =
          gdn_thread_info_new_from_scm (scm_c_vector_ref (s_rows, i));
      if (info != NULL)
        g_ptr_array_add (items, info);
    }

  /* Probably called from the Guile thread; GListModels can only be
   * updated from the GTK thread, so the update is sent there. */
  xgtk_list_store_replace_all_on_main (self->store, items);
  return SCM_UNSPECIFIED;
}

void
gdn_thread_view_guile_init (void)
{
  SCM name, slots;

  name = scm_from_utf8_symbol ("gdn-thread-view");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  scm_thread_view_type = scm_make_foreign_object_type (name, slots, NULL);

  scm_c_define_gsubr ("gdn-update-threads!", 2, 0, 0, scm_update_threads_x);
  scm_c_export ("gdn-update-threads!", NULL);
}
