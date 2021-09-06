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

typedef enum
{
  GDN_APPLICATION_MODE_UNKNOWN,
  GDN_APPLICATION_MODE_GUIDED,
  GDN_APPLICATION_MODE_REPL
} GdnApplicationMode;

struct _GdnApplication
{
  GtkApplication parent_instance;
  gulong         activate_handler_id;
  GdnApplicationMode mode;
  int            argc;
  char **        argv;
  char *         args;
};

G_DEFINE_TYPE (GdnApplication, gdn_application, GTK_TYPE_APPLICATION)

GdnApplication *_default = NULL;

static void
gdn_application_init (G_GNUC_UNUSED GdnApplication *app)
{
#ifdef REPL_MODE
  app->mode = GDN_APPLICATION_MODE_REPL;
#else
  app->mode = GDN_APPLICATION_MODE_GUIDED;
#endif
}

static void
application_finalize (GObject *object)
{
  GdnApplication *self = GDN_APPLICATION (object);
  g_strfreev (self->argv);
  self->argv = NULL;
  self->argc = 0;
  self->mode = GDN_APPLICATION_MODE_UNKNOWN;

  /* Don't forget to chain up. */
  G_OBJECT_CLASS (gdn_application_parent_class)->finalize (object);
}

static void
gdn_application_class_init (GdnApplicationClass *klass)
{
  GObjectClass *gobj_class = G_OBJECT_CLASS (klass);

  gobj_class->finalize = application_finalize;
}

/* The app's "activate" signal handler */
static void
on_activate (GdnApplication *app)
{
  GtkWindow *window;

  /* Get the current window or create one if necessary. */
  window = gtk_application_get_active_window (GTK_APPLICATION (app));
  if (window == NULL)
    window = g_object_new (GDN_TYPE_APPLICTION_WINDOW, "application", app,
                           "default-width", 700, "default-height", 400, NULL);
  gtk_window_present (window);
}

GdnApplication *
gdn_application_new (void)
{
  GObject *       gobj;
  GdnApplication *app;
  unsigned        flags;

  g_set_application_name ("Guidance");
  flags = G_APPLICATION_NON_UNIQUE;
  gobj = g_object_new (gdn_application_get_type (), "application-id",
                       "com.lonelycactus.Guidance", "flags", flags, NULL);
  app = GDN_APPLICATION (gobj);
  g_signal_connect_data ((gpointer) app, "activate", G_CALLBACK (on_activate),
                         NULL, NULL, 0);

  _default = app;
  return app;
}

GdnApplication *
gdn_application_get_default (void)
{
  return _default;
}
