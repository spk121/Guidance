/* guidance-application.c
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

#include "guidance-application.h"
#include "guidance-application-window.h"
#include "guidance-config.h"

struct _GdnApplication
{
  GtkApplication parent_instance;
  gulong         activate_handler_id;
};

G_DEFINE_TYPE (GdnApplication, gdn_application, GTK_TYPE_APPLICATION)

static void
gdn_application_init (G_GNUC_UNUSED GdnApplication *app)
{
}

static void
gdn_application_class_init (G_GNUC_UNUSED GdnApplicationClass *klass)
{
}
static void
on_activate (GdnApplication *app)
{
  GtkWindow *window;

  /* Get the current window or create one if necessary. */
  window = gtk_application_get_active_window (GTK_APPLICATION (app));
  if (window == NULL)
    window = g_object_new (GDN_TYPE_APPLICTION_WINDOW,
                           "application", app,
                           "default-width", 600,
                           "default-height", 300,
                           NULL);
  gtk_window_present (window);
}

GdnApplication *
gdn_application_new (void)
{
  GdnApplication *app;

  g_set_application_name ("Guidance");
  app = GDN_APPLICATION (g_object_new (gdn_application_get_type (),
                                       "application-id", "com.lonelycactus.Guidance",
                                       "flags", G_APPLICATION_FLAGS_NONE,
                                       NULL));
  app->activate_handler_id = g_signal_connect_data ((gpointer) app, "activate", G_CALLBACK (on_activate),
                                                    NULL, NULL, 0);
  return app;
}
