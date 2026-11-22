Guidance: Plan to Completion
============================

Assessment date: 2026-11-22.  Tested on Ubuntu 24.04 with GTK 4.14.5
and Guile 3.0.9, driven headless under Xvfb with screenshots.

Current State
-------------

### Build

- `guidance.gresource.xml` references `gdn/module-text.scm`, which
  does not exist, so the build fails immediately.
- With that removed it compiles and links, with 21 warnings.  Several
  are `-Wincompatible-pointer-types`, which GCC 14 makes an error by
  default.

### Runtime

| Area                                            | Status |
|-------------------------------------------------|--------|
| Window, sidebar, toolbar, terminal, `repl>`     | Works |
| Evaluate expressions; errors enter Error mode   | Works |
| Run button loads the command-line file          | Works |
| Threads page, Environment page                  | Works |
| Startup with no login session                   | Crash: `getlogin` returns `#f`, passed to `scm_to_utf8_string` |
| Backtrace page                                  | Crash on GTK >= 4.12 (cells are `GtkColumnViewCell`, a `GtkListItem` subclass); shows Guile-internal frames rather than user frames |
| `_break_`, traps, stepping                      | Trap registered but never fires |
| Modules page                                    | Empty: `gdn_module_view_guile_init` never called |
| Stop button in Trap mode                        | No `'stop` case in `trap-handler` |
| Error-mode input                                | 1 s latency from `(sleep 1)` in loop |
| Peek log, Pause button (from README)            | Not implemented |

### Why traps never fire

Verified in plain Guile 3.0.9.  Traps fire only when all of these hold:

1. The debug VM engine is active.  `main.c` already sets this as the
   default for subsequently spawned threads.
2. The VM trace level is > 0.  `with-default-trap-handler` sets it
   from the count of enabled traps *at entry*; traps added afterwards
   (as `_break_` and GUI-added traps are) leave it at 0.
3. The code is compiled.  In interpreted code all closures share the
   evaluator's code range, so a procedure trap fires on every call.

### Threading

The Guile thread touches GTK directly in `gdn-update-backtrace!`
(mutates a `GListStore`) and `gdn-set-input-mode!` (sets button
sensitivity).  The other views correctly defer to the GTK thread with
`g_idle_add`.

### Housekeeping

- `src/guidance-emoji.o` is committed.
- Orphaned files: `src/gtk/terminal-page.ui`,
  `src/guidance-threads-view.h`, `src/guidance-window.[ch]`.
- `po/POTFILES` lists non-existent files.
- `guidance-module-info.c` is listed twice in `src/meson.build`.
- ASan/UBSan and `-fanalyzer` are forced on for every GCC build.
- Flatpak manifest uses the EOL GNOME 40 runtime and a
  `file:///home/mike/...` source; CI installs GTK 3 and targets
  `master` only.

Decisions (assumed; revisit if needed)
--------------------------------------

1. Keep the current architecture: Guile runs on a worker thread in the
   same process, talking to GTK over PTYs.
2. Minimum versions: GTK >= 4.10, Guile >= 3.0.9.
3. Procedure breakpoints are supported for compiled code; loaded files
   are auto-compiled.

Plan
----

### Phase 0: Builds and runs cleanly

- [x] Remove the `module-text.scm` resource reference.
- [x] Tolerate `#f` from `getlogin` and other environment queries.
- [x] Fix the list-item type assertion for GTK >= 4.12.
- [x] Fix all compiler warnings.
- [x] Remove the stray object file and orphaned sources; fix POTFILES
      and the duplicate source entry.
- [x] Make sanitizers and `-fanalyzer` meson options, off by default.
- [x] Complete the AppStream metadata so `meson test` passes.

Phase 0 notes: sanitizers now use meson's built-in option
(`-Db_sanitize=address,undefined`); the analyzer is `-Danalyzer=true`.
Minimum versions raised to GTK 4.10 and Guile 3.0.9.  Incompatible
pointer types and implicit declarations are now errors.

### Phase 1: Working trap/step loop

- [x] Keep the VM trace level correct whenever traps are added,
      enabled, or disabled, so that breakpoints fire.
- [x] Make `_break_` stop at its call site.
- [x] Add `'stop` to the trap handler, aborting to a prompt that
      returns to Eval mode.  Remove `sleep` from the error loop.
- [x] Marshal every GTK update from the Guile thread to the GTK
      thread.
- [x] Start backtraces at the user's frame.
- [x] Replace the 8-slot `trap-thunk-N` ring with a single queue.
- [x] Example `.scm` programs plus an automated headless smoke test.
- [x] Stop interrupts a running program (pulled forward from Phase 2).
- [x] Keyboard shortcuts: F5 run/continue, Shift+F5 stop, F10 step
      over, F11 step into, Shift+F11 step out.

Phase 1 notes:

- How it works: `run-under-debugger` in `(gdn repl)` wraps every run
  in a stop prompt, the trap handler, a pre-unwind error handler, and
  a stack prompt used to cut backtraces.  It keeps the VM trace level
  at 1 while the program runs.  `_break_` is a low-level procedure
  trap installed for each run, so it pauses through a VM hook like
  every other trap.  Step and Step Into go line by line, built on
  `trap-matching-instructions`, and skip Guile's own library code.
  Step Out pauses in the caller and reports the return values.
- The GTK thread sends the REPL operator actions through a queue, and
  other requests (such as adding a trap) through a second queue.
  While a program runs, it interrupts the REPL thread with a single
  async, `gdn-async-poll`.  A stop flag keeps a late async from
  stopping a later program.
- The Scheme modules are compiled when they are loaded, because
  Guile's procedure traps cannot tell interpreted procedures apart.
  This costs about a second at startup; installing the modules where
  Guile can cache the compiled code would avoid it.
- Tests: `meson test` runs `tests/repl-test.scm`, which tests the
  debugger core in plain Guile with the GUI stubbed out
  (`tests/stubs`), and `tests/gui-smoke.sh`, which drives the real app
  on Xvfb using a terminal transcript (`GUIDANCE_TRANSCRIPT`).  The
  smoke test is skipped if Xvfb or xdotool is missing.
- Known cosmetic issue: output and error text arrive through separate
  pseudoterminals, so their relative order in the terminal can vary.

### Phase 2: Finish the panels

- [ ] Modules page: register the module view and hook module loading.
      Adding a trap from it should use `gdn_lisp_add_proc_trap`.
- [ ] Backtrace frame click jumps to source; set breakpoints from a
      source line.
- [ ] Enable/disable/delete traps from the Traps page, as new request
      types handled by `process-request` in `(gdn repl)`.
- [ ] Pause: interrupt a running program and enter trap mode, using
      the same async as Stop.
- [ ] Evaluate expressions in error and trap mode in the paused
      frame's environment, so local variables are visible.

### Phase 3: README features

- [ ] Peek log tab (`_pk_`).
- [ ] Pass command-line arguments through the Run button.
- [ ] Rewrite the README to match reality.

### Phase 4: Packaging

- [ ] Current GNOME runtime and Guile in the Flatpak manifest.
- [ ] GitHub Actions on the current branch layout.
- [ ] Refresh the Dockerfile.
