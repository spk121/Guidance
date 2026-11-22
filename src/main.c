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

#include <gtk/gtk.h>
#include <libguile.h>
#include <libintl.h>
#include <stdio.h>
#include <string.h>

#include "guidance-application.h"
#include "guidance-config.h"

static const char usage[] =
    "Usage: guidance [OPTION]... [FILE [ARG]...]\n"
    "Debug a GNU Guile program.  Press Run (F5) to run FILE with the ARGs,\n"
    "as `guile FILE ARG...' would.\n"
    "\n"
    "  -L DIRECTORY   add DIRECTORY to the front of the module load path\n"
    "  -C DIRECTORY   like -L, but for compiled files\n"
    "  -l FILE        load source code from FILE before running the program\n"
    "  -s FILE        the program to run; the remaining arguments are its ARGs\n"
    "  -c EXPR        the program to run is the expression EXPR\n"
    "  -e FUNCTION    after loading the program, call FUNCTION with the\n"
    "                 program's command line, as a list of strings\n"
    "  --             stop scanning options; the next argument is FILE\n"
    "  -h, --help     display this help and exit\n"
    "  -v, --version  display version information and exit\n";

/* Scans Guidance's options, by the same rules as gdn-parse-command-line
 * in (gdn lib), for --help and --version.  Everything else is parsed
 * in Scheme.  Options end at the program, so `guidance foo.scm --help'
 * passes --help to foo.scm. */
static gboolean
handle_info_options (int argc, char **argv)
{
  for (int i = 1; i < argc; i++)
    {
      const char *arg = argv[i];

      if (strcmp (arg, "-h") == 0 || strcmp (arg, "--help") == 0)
        {
          fputs (usage, stdout);
          return TRUE;
        }
      if (strcmp (arg, "-v") == 0 || strcmp (arg, "--version") == 0)
        {
          printf ("guidance %s\n", PACKAGE_VERSION);
          printf ("Using Guile %d.%d.%d and GTK %u.%u.%u\n", SCM_MAJOR_VERSION,
                  SCM_MINOR_VERSION, SCM_MICRO_VERSION, gtk_get_major_version (),
                  gtk_get_minor_version (), gtk_get_micro_version ());
          return TRUE;
        }
      if (strcmp (arg, "-L") == 0 || strcmp (arg, "-C") == 0
          || strcmp (arg, "-l") == 0 || strcmp (arg, "-e") == 0)
        i++; /* skip the option's argument */
      else if (strcmp (arg, "-s") == 0 || strcmp (arg, "-c") == 0
               || strcmp (arg, "--") == 0 || arg[0] != '-' || arg[1] == '\0')
        break; /* the program and its arguments follow */
    }
  return FALSE;
}

/* Guile keeps the files it compiles automatically in
 * $XDG_CACHE_HOME/guile/ccache, and uses one that is there in
 * preference to compiling again.  Guidance compiles programs
 * differently from Guile, to keep them debuggable: see
 * %debug-compile-options in (gdn repl).  A file compiled by Guile
 * itself would hide that, so Guidance has a cache of its own, under
 * $XDG_CACHE_HOME/guidance.
 *
 * Guile works out where its cache is once, when it starts.  So the
 * variable is changed only for that long, and the program being
 * debugged, and GTK, see the value it had. */
static char *original_cache_home = NULL;

static void
use_own_compiled_cache (void)
{
  const char *cache_home = g_getenv ("XDG_CACHE_HOME");
  char *      own;

  original_cache_home = g_strdup (cache_home);
  if (cache_home != NULL && *cache_home != '\0')
    own = g_build_filename (cache_home, "guidance", NULL);
  else
    own = g_build_filename (g_get_home_dir (), ".cache", "guidance", NULL);
  g_setenv ("XDG_CACHE_HOME", own, TRUE);
  g_free (own);
}

static void
restore_cache_home (void)
{
  if (original_cache_home != NULL)
    g_setenv ("XDG_CACHE_HOME", original_cache_home, TRUE);
  else
    g_unsetenv ("XDG_CACHE_HOME");
  g_clear_pointer (&original_cache_home, g_free);
}

static void
inner_main (G_GNUC_UNUSED void *data, int argc, char **argv)
{
  GdnApplication *app = NULL;
  int             ret;

  /* Guile has started. */
  restore_cache_home ();

  /* Set up gettext translations */
  bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
  bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
  textdomain (GETTEXT_PACKAGE);

  if (handle_info_options (argc, argv))
    scm_primitive_exit (scm_from_int (0));

  /* The command-line arguments are Guile's, available to Scheme as
   * (command-line); GApplication gets none, so that it doesn't try to
   * interpret the program's arguments. */
  app = gdn_application_new ();
  ret = g_application_run (G_APPLICATION (app), 1, argv);
  scm_primitive_exit (scm_from_int (ret));
}

int
main (int argc, char **argv)
{
  scm_c_set_default_vm_engine_x (1);
  use_own_compiled_cache ();
  scm_boot_guile (argc, argv, inner_main, NULL);
  return 0;
}
