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
  Guidance does now.
- Guidance's own modules count as library code: their frames are
  hidden from backtraces and skipped by stepping, also when they are
  loaded from files, as in the tests.

### Phase 4: Packaging

Packaging is dropped: Guidance is built from source with Meson.  The
old CI workflow, its packaging manifests, and the Dockerfile were
removed.

- [x] A GitHub Actions workflow, `.github/workflows/build.yml`, that
      builds Guidance on Ubuntu 24.04 and runs `meson test`, in one job
      as is and in another with ASan and UBSan.  On failure, it keeps
      the test log as an artifact.

### Phase 5: Alpha release

Assessment date: 2026-11-22, on Ubuntu 24.04 with GTK 4.14.5 and
Guile 3.0.9.  The build has no warnings and `meson test` passes (5 of
5).  Beyond the test suite, Guidance was driven under Xvfb with
keyboard, mouse, and screenshots through some forty scenarios that the
smoke test does not cover.  The findings are below, most serious
first.  Each was reproduced in the running application unless marked
"(from the code)".

Each fix should come with a test, in `tests/repl-test.scm` where the
debugger core is enough, otherwise in `tests/gui-smoke.sh`.

#### Blockers: fix before an alpha

- [ ] **A pending step survives the end of its run and breaks the
      next one.**  Step (or Step Out), then let the run end before the
      step completes: by an error on the stepped line, by Stop, or by
      stepping past the end of the program.  The next Run, or the next
      expression at `repl>`, fails with "In procedure make-stack:
      Prompt tag not found while narrowing stack" and does not run.
      It works again after that one failure.  The one-shot traps made
      by `add-ephemeral-trap!` are only disabled when they fire.
      `run-under-debugger` should disable any that are left, on every
      way out.
- [ ] **An expression that does not finish, evaluated while paused,
      hangs Guidance for good.**  At `break>` or `error>`, enter
      `(let loop () (loop))`.  Stop and Pause do nothing, nor does any
      later input; the window must be closed.  In those modes Stop is
      queued for the REPL thread, which is busy, instead of being
      delivered asynchronously as it is in Run mode.
- [ ] **Stop and Pause do nothing while the program waits for
      input.**  With a program blocked in `read-line`, neither takes
      effect (waited 20 s) until a line is entered.  The blocking read
      on the pseudoterminal does not notice asyncs.  The input port
      needs an interruptible wait.
- [ ] **Actions pressed in the wrong mode are queued and replayed.**
      The shortcuts and toolbar actions are not tied to the mode: only
      the buttons' sensitivity is.  Pressing F5 twice runs the program
      a second time as soon as the first run ends, including straight
      after Stop.  F5 or F10 pressed while the program runs is used up
      by its next pause, which then continues by itself.  Enable the
      actions per mode, and have the REPL thread discard actions that
      were queued in an earlier mode.
- [ ] **The Terminal loses output.**  `xread` in
      `guidance-terminal-view.c` keeps at most 10 KiB per read, drops
      the rest, inserts "■", and logs a CRITICAL.  A program printing
      in a loop lost text 15 times in 5 seconds.  The text buffer also
      grows without limit: the same program took Guidance to 314 MB in
      those 5 seconds.  Read everything, and cap the scrollback.
- [ ] **Local variables lose their names.**  Programs are compiled at
      Guile's default optimization level, so a `let` variable `a` in
      a ten-line procedure is listed on the Backtrace page as `tmp`,
      next to compiler temporaries named `box` and `arg`, and
      entering `a` at the prompt gives "Unbound variable: a".
      (`tests/programs/locals.scm` happens to keep its names.)
      Verified in plain Guile: compiled with `#:optimization-level 1`
      the same procedure reports `a` at every line.  Compile programs
      run under the debugger at a low optimization level.  Files
      already in Guile's compiled-file cache were compiled at the
      default level, so this needs Guidance's own cache or a forced
      recompile.  At level 1, check line stepping again: the line
      table then revisits the `let` line between statements.
- [ ] **Procedure breakpoints stop working when the program is run
      again.**  A trap made from the Modules page or with
      `add-trap-at-procedure-call!` belongs to the procedure object.
      Running the program again redefines its procedures, and the
      trap, still listed as active on the Traps page, never fires
      again.  Traps on procedures in modules that are not reloaded
      keep working.  Record these traps by module and name and
      re-attach them at each run, as line breakpoints already do.
- [ ] **Stepping leaves the program.**  Step Out of a top-level form
      pauses in `boot-9.scm:4388`, inside `load`, and further steps
      stay in Guile's internals.  Step at the last line of the program
      gives "Step to an anonymous procedure", twice: these are
      Guidance's own frames, which have no file name because its
      modules are compiled from strings, so
      `guile-internal-frame?` does not recognize them.  Stepping
      should only pause in frames inside the stack prompt, and a step
      that leaves the program should just let it finish.
- [ ] **The menu button does nothing.**  It is a plain `GtkButton`.
      `menus.ui` is not used, and names actions that do not exist
      (`win.repl-load`, `win.repl-history-next`, `app.quit`).  There
      is no About dialog and no Quit, and Ctrl+Q does nothing.  The
      program can only be named on the command line, so Guidance
      started from its desktop entry is a REPL with nothing to run.
      Give the menu at least Open Program, Keyboard Shortcuts (the
      overlay exists and works on Ctrl+?), About, and Quit.

#### Should fix

- [ ] **Other threads are outside the debugger.**  `_break_` in a
      thread made with `call-with-new-thread` is ignored without a
      word.  An error there prints Guile's own backtrace to the
      Terminal and the thread dies; the debugger only notices the
      knock-on error in the main thread.  Traps and the error handler
      are installed for the REPL thread alone.  For an alpha, at
      least report such errors cleanly and document the limit.
- [ ] **Runaway recursion never ends.**  `(define (deep n) (+ 1 (deep
      (+ n 1))))` reaches "stack overflow" in 13 s in plain Guile.
      Under Guidance it was still running after 150 s.  Stop and Pause
      do work.  Set a stack limit with
      `call-with-stack-overflow-handler` and enter Error mode.
- [ ] **Programs run about 25 times slower.**  `(fib 30)` takes 27 ms
      in plain Guile and 680 ms under Guidance, because the VM trace
      level is 1 for the whole run.  Worth a look at whether hooks
      can be off when no trap, breakpoint, or step is active; if not,
      say so in the README.
- [ ] **Subprocess output misses the Terminal.**  Only Guile's ports
      are redirected, not file descriptors 0 to 2, so the output of
      `(system "echo hi")` goes to the terminal Guidance was started
      from, or nowhere.
- [ ] **No end-of-file for standard input.**  Ctrl+D in the entry does
      nothing, so a program that reads until EOF can only be stopped.
- [ ] **The entry takes one line, and has no history.**  An unfinished
      expression is a read error ("unexpected end of input"), so
      nothing longer than a line can be entered.  The Up key does
      nothing: the history list in `guidance-terminal-view.c` is
      filled but never read, and its duplicate check compares only a
      prefix (from the code).  The `GtkMenuButton` beside the entry
      is empty.
- [ ] **Backtrace page layout.**  At the default window size the Value
      column, the one that matters, is off the right edge, and the
      Location column is cut to "fact.sc".  The Type column is blank
      except for "A" on arguments.  A top-level frame is shown as
      `(_)`.
- [ ] **Source page.**  No line numbers, and no mark on lines that
      have breakpoints (already noted under Phase 2).
- [ ] **Threads page.**  Rows read `#<thread 139940890986176
      (7f46c934c6c0)>` with an emoji as ID.  Nothing says which is the
      program's thread, and Guidance's own threads are listed.
- [ ] **Compiler chatter and ordering in the Terminal.**  The first
      run of each file prints ";;; note: auto-compilation is enabled
      ...", ";;; compiling ..." and ";;; compiled ..." with long cache
      paths.  Because output and errors use separate pseudoterminals
      (noted under Phase 1), ";;; compiled" can arrive after "Break
      at", an error message after the `error>` prompt, and in one
      run a thread's backtrace landed in the middle of a line.  Hide
      the compiler notes, and merge the two streams in order.
- [ ] **Program output is not monospaced.**  Only echoed input and
      error text get the monospace tag.
- [ ] **Startup warnings.**  On Guile 3.0.9 Guidance prints
      "possibly unbound variable `total-processor-count'" and
      "`current-processor-count'" to its own stderr at every start,
      and the Environment page shows 0 for both; `(gdn lib)` needs
      `(ice-9 threads)`.  Startup takes 1.7 s, most of it compiling
      the Scheme modules (noted under Phase 1).
- [ ] **Options.**  Guile options that Guidance does not know, such as
      `--no-auto-compile` and `--debug`, are reported as unrecognized
      after the first prompt.  Accept the harmless ones.  `-e` only
      takes a plain symbol, not Guile's `(@ (module) name)` form (from
      the code).
- [ ] **Leftovers.**  The last 340 lines of `src/gdn/lib.scm` are a
      commented-out copy of an old REPL.  `menus.ui` and the history
      fields are unused.  Tooltips are lower case and do not give the
      shortcuts ("run process" rather than "Run (F5)").
- [ ] **Release housekeeping.**  The AppStream metadata lists release
      0.1.0 dated 2021-12-09 and has no screenshot.  There is no NEWS
      file.  Update the README's status paragraph and its list of
      limits to match what is decided above.

#### Limits to document rather than fix

- Procedure traps on interpreted procedures, such as one made with
  `eval`, work but report their location as `eval.scm:333`.
- A file with a read error is run by the interpreter up to the error,
  so its earlier forms take effect and its breakpoints do not.
- `(primitive-exit)` ends Guidance itself.  `(exit)` and `(quit)` end
  only the program, as intended.
- Input typed before the program asks for it is kept, as on a
  terminal.  So is input left over from a program that was stopped,
  which the next program then reads.

#### Checked and working

Break, Step, Step Into, Step Out with return values, Continue, Stop,
and Pause (also during `sleep`); Error mode for errors and `throw`,
and not for exceptions the program handles itself (`catch`,
`false-if-exception`, `with-exception-handler`, `guard`); evaluation
in the selected frame; line breakpoints with F9, kept across runs;
the Traps page; the Modules page, expanding a module and trapping a
procedure; stepping into a module in another file, found through a
load path the program adds; a script with a `#!` header, a relative
path, and non-ASCII strings; `-c`, `-l`, `-L`, `-e`, and program
arguments; reloading a source file that changed between runs; the
Peek page; circular lists and a 200,000-element list as arguments;
the shortcuts overlay; closing the window while the program runs or is
paused (exit status 0); `meson install`.
