/* guidance-terminal-view.h
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

#pragma once

#include <gtk/gtk.h>
#include <libguile.h>

G_BEGIN_DECLS
#define GDN_TYPE_TERMINAL_VIEW (gdn_terminal_view_get_type ())

G_DECLARE_FINAL_TYPE (
    GdnTerminalView, gdn_terminal_view, GDN, TERMINAL_VIEW, GtkBox)

void gdn_terminal_view_connect_ports (GdnTerminalView *self,
                                      int              input_fd,
                                      int              prompt_fd,
                                      int              error_fd,
                                      int              output_fd);
void gdn_terminal_view_disconnect_lisp_ports (GdnTerminalView *view);

G_END_DECLS
