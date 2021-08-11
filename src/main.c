/* main.c
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

#include <glib/gi18n.h>
#include <libguile.h>

#include "guidance-application.h"
#include "guidance-config.h"
#include "guidance-window.h"

static void
inner_main (G_GNUC_UNUSED void *data, int argc, char **argv)
{
  GdnApplication *app = NULL;
  int ret;

  /* Set up gettext translations */
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);

  app = gdn_application_new ();
  ret = g_application_run (G_APPLICATION (app), argc, argv);
  scm_primitive_exit (scm_from_int (ret));
}

int
main (int argc, char **argv)
{
  scm_c_set_default_vm_engine_x (1);
  scm_boot_guile (argc, argv, inner_main, NULL);
  return 0;
}
