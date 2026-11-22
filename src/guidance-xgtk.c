/* guidance-xgtk.c
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

#include "guidance-xgtk.h"

void
xgtk_button_set_sensitive (GtkButton *button, gboolean flag)
{
  g_assert (GTK_IS_BUTTON (button));
  gtk_widget_set_sensitive (GTK_WIDGET (button), flag);
}

void
xgtk_invoke_on_main (GSourceFunc func, gpointer data, GDestroyNotify notify)
{
  /* Idle sources of equal priority are dispatched in the order they
   * were added, which keeps the GUI updates in order. */
  g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, func, data, notify);
}

typedef struct
{
  GListStore *store;
  GPtrArray * items;
} StoreReplacement;

static gboolean
replace_all (gpointer user_data)
{
  StoreReplacement *r = user_data;
  g_list_store_splice (r->store, 0,
                       g_list_model_get_n_items (G_LIST_MODEL (r->store)),
                       r->items->pdata, r->items->len);
  return G_SOURCE_REMOVE;
}

static void
store_replacement_free (gpointer user_data)
{
  StoreReplacement *r = user_data;
  g_object_unref (r->store);
  g_ptr_array_unref (r->items);
  g_free (r);
}

void
xgtk_list_store_replace_all_on_main (GListStore *store, GPtrArray *items)
{
  g_assert (G_IS_LIST_STORE (store));
  g_assert_nonnull (items);

  StoreReplacement *r = g_new0 (StoreReplacement, 1);
  r->store = g_object_ref (store);
  r->items = items;
  xgtk_invoke_on_main (replace_all, r, store_replacement_free);
}
