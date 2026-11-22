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
- The CI workflow was out of date: it installed GTK 3, used an
  end-of-life GNOME runtime, and ran only on `master`.

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

- [x] Modules page: lists loaded modules; expanding one lists its
      procedures; each can be opened in the Source page or trapped.
- [x] Backtrace frame click selects the frame (variables, and the
      frame for evaluation); its location opens the Source page.
- [x] Set breakpoints from a source line: "Set Breakpoint Here" in the
      Source page's context menu, or F9.  The Source page follows the
      paused program.
- [x] Enable/disable/delete traps from the Traps page.
- [x] Pause (F6 or the toolbar): interrupt a running program and enter
      trap mode.
- [x] Evaluate expressions in error and trap mode with the selected
      frame's local variables in scope.

Phase 2 notes:

- The GTK thread never reads Guile data.  The REPL sends plain data
  (strings and numbers) for every page, and the GUI sends requests
  that name things by value (a module and procedure name, a file and
  line, a trap index, a frame index).  See `process-request` in
  `(gdn repl)` and the `gdn_lisp_request_*` functions.
- Line breakpoints are Guidance's own, in `(gdn lib)`.  Guile's
  `add-trap-at-source-location!` relies on an index that goes stale
  after `load`, and only covers code loaded when the trap is made.
  Guidance's attach to loaded procedures, and to any other procedure
  with code at the line when it is first called.  So breakpoints can
  be set before Run, and survive running the program again.  They
  match exact lines: a line with no code gets a note instead.
- A breakpoint set on the paused line does not stop again when the
  program continues back into that line.
- Expressions see copies of the frame's locals: assigning to them
  does not change the program.  Which locals exist at a given point is
  up to Guile's liveness information; for example, a variable's last
  use may already have consumed it.
- Backtraces show at most 500 frames.
- Requests are handled with asyncs blocked, so that a burst of them
  (as from the Modules page) cannot nest and starve the program.
- Not yet: highlighting breakpoint lines in the Source page.  Nested
  procedures can have line breakpoints, but the Modules page lists
  only procedures bound at a module's top level.

### Phase 3: README features

- [x] Peek log tab (`_pk_`).
- [x] Pass command-line arguments through the Run button.
- [x] Rewrite the README to match reality.

Phase 3 notes:

- Guidance takes a subset of Guile's options: `-L`, `-C`, `-l`, `-s`,
  `-c`, `-e`, `--`, `-h`/`--help`, `-v`/`--version`.  They are parsed
  in `(gdn lib)` (`gdn-parse-command-line`); `main.c` handles `--help`
  and `--version` before the GUI starts, scanning by the same rules,
  and gives GApplication no arguments.  Run sets the program's
  `(command-line)` as Guile would: `("FILE" "ARG" ...)`.
- `_pk_` logs to the Peek page with the caller's location and returns
  its last argument, like `pk`.
- The README keeps the Philosophy section; the rest describes what
  Guidance does now.  Its Docker section is marked as out of date
  until Phase 4.
- Guidance's own modules count as library code: their frames are
  hidden from backtraces and skipped by stepping, also when they are
  loaded from files, as in the tests.

### Phase 4: Packaging

Dropped: Guidance is built from source with Meson.  The old CI
workflow and its packaging manifests were removed, so there is no CI.

- [ ] Optionally, a GitHub Actions workflow that builds Guidance and
      runs `meson test`.
- [ ] The Dockerfile and the README's Docker section: decide whether
      to keep them.
