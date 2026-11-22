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

Status: all the blockers are fixed, each with tests.

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

The open one is last in this list.

- [x] **A pending step survives the end of its run and breaks the
      next one.**  Step (or Step Out), then let the run end before the
      step completes: by an error on the stepped line, by Stop, or by
      stepping past the end of the program.  The next Run, or the next
      expression at `repl>`, failed with "In procedure make-stack:
      Prompt tag not found while narrowing stack" and did not run.
      Fixed: `(gdn repl)` keeps the one-shot traps that have not
      fired in `*pending-steps*`, and `run-under-debugger` disables
      them on every way out (`cancel-pending-steps!`).  Three tests
      in `tests/repl-test.scm` cover the error, Stop, and
      end-of-program cases.
- [x] **An expression that does not finish, evaluated while paused,
      hangs Guidance for good.**  At `break>` or `error>`, enter
      `(let loop () (loop))`.  Stop and Pause did nothing, nor did any
      later input; the window had to be closed.  In those modes Stop
      was queued for the REPL thread, which was busy.  Fixed: Stop
      now always interrupts the REPL thread, and in those modes wakes
      it too, in case it is waiting for the operator.  An expression
      entered while paused runs inside its own prompt, and Stop
      aborts to that: "The expression was stopped." is printed, and
      the program stays paused where it was.  A second Stop stops
      the program.  Tests: two in `tests/repl-test.scm`, one in
      `tests/gui-smoke.sh`.
- [x] **Stop and Pause do nothing while the program waits for
      input.**  With a program blocked in `read-line`, neither took
      effect until a line was entered: a thread blocked in a read
      does not run asyncs.  Fixed with Guile's suspendable ports:
      `(gdn repl)` calls `install-suspendable-ports!`, makes the
      program's standard input non-blocking, and sets a
      `current-read-waiter` that waits for it a tenth of a second at
      a time, in Scheme, where asyncs run.  Other ports keep the
      waiter they had.  `read-line`, `read`, `read-char`, `get-line`
      and the other port procedures that suspendable ports cover can
      all be interrupted.  Output speed is unchanged.  Tests: two in
      `tests/repl-test.scm`, whose stub for standard input is now a
      pipe, and one in `tests/gui-smoke.sh`.

      Still open:
      - An expression entered while paused cannot be given input: the
        entry is the program's standard input only in Run mode.
        `(read-line)` at `break>` waits until Stop.
      - Stop does not end the program's other threads.  One left
        waiting for input takes some of what is typed for the next
        run.  This belongs with "Other threads are outside the
        debugger" below.
- [x] **Actions pressed in the wrong mode are queued and replayed.**
      The shortcuts and toolbar actions were not tied to the mode:
      only the buttons' sensitivity was.  Pressing F5 twice ran the
      program a second time as soon as the first run ended, including
      straight after Stop.  F5 or F10 pressed while the program ran
      was used up by its next pause, which then continued by itself.
      Fixed, in three parts:
      - The actions are enabled by mode (`set_actions_for_mode`), and
        the buttons and shortcuts both follow them.
      - Each handler also checks the mode the REPL thread last
        reported, since the actions only catch up when the GTK thread
        is next idle.
      - When a run ends, actions still queued for the REPL thread are
        dropped (`gdn_lisp_discard_actions`).  This covers Run pressed
        twice before the REPL thread takes up the first.  Expressions
        the operator entered are kept.

      So that a key pressed the moment a pause is announced is not
      lost, the REPL now reports the new mode before it prints the
      message, and the GTK thread applies it ahead of terminal output
      and key presses.  Tests: two cases in `tests/gui-smoke.sh`,
      with `tests/programs/late-break.scm`.
- [x] **The Terminal loses output.**  `xread` in
      `guidance-terminal-view.c` kept at most 10 KiB per read, dropped
      the rest, inserted "■", and logged a CRITICAL.  A program
      printing in a loop lost text 15 times in 5 seconds.  The text
      buffer also grew without limit: the same program took Guidance
      to 314 MB in those 5 seconds.  Fixed:
      - Everything is read.  Each time a stream is ready, up to 64 KiB
        is read from it; what is left keeps it ready.  The streams are
        read when the GTK thread is otherwise idle, so a program that
        prints without pause is held up by its full pseudoterminal
        rather than freezing the window.  Stop still acts at once.
      - The terminal keeps the latest 10,000 lines, or 2,000,000
        characters.  Past that, the oldest text is dropped, in whole
        lines, down to 9,000 lines or 1,800,000 characters
        (`xgtk_text_buffer_drop_old_text`).  The Peek log has the same
        limit.
      - A character of several bytes that is split between two reads
        is put back together.  Bytes that are not UTF-8, and NULs,
        are shown as "�" instead of cutting the output short.
      - A line of more than 1,000 characters is shown broken into
        lines of that length.  One 4,000,000-character line otherwise
        took Guidance to 2.2 GB and hung it.
      - Output is added at the end of the text, and the view is no
        longer editable: typing in it used to change the text, and
        output went in wherever the cursor was.

      Measured: 30,000 lines arrive complete in about a second; an
      endless printing loop ran 6 seconds at a steady 277 MB (260 MB
      is the size with nothing printed) and stopped at once, with
      116,255 lines received and none missing; the 4,000,000-character
      line arrives complete in 12 seconds.  Test: one case in
      `tests/gui-smoke.sh`, with `tests/programs/flood.scm`.

      A prompt is now shown after the output that the REPL printed
      before it: the prompt comes by a stream of its own, and could
      overtake the last of a large output.  The terminal's reads no
      longer block.

      Still open: the terminal always shows the end when something
      is printed, even if the operator has scrolled back to read.

      Fixed later: the prompt stream could run two prompts together,
      as "step>repl>", when a run ended just after a pause.  Each
      prompt now ends with a newline, and the terminal shows the last
      complete one.
- [x] **Local variables lose their names.**  Programs were compiled
      at Guile's default optimization level, so a `let` variable `a`
      in a ten-line procedure was listed on the Backtrace page as
      `tmp`, next to compiler temporaries named `box` and `arg`, and
      entering `a` at the prompt gave "Unbound variable: a".  Fixed:
      - Guidance compiles with four optimizations off
        (`%debug-compile-options` in `(gdn repl)`): partial
        evaluation, simplification, common subexpression elimination,
        and dead-code elimination.  This applies to the programs it
        loads and the modules they use, through
        `%auto-compilation-options`, and to the operator's
        expressions.  The list was found by experiment on Guile
        3.0.9, and may need revisiting for other versions.
      - Optimization level 1, the first idea, keeps the names too, but
        its line table returns to the first line of a body after each
        statement, which spoils stepping.  With the four off, steps
        follow the lines, and procedures are no longer inlined into
        their callers.
      - Guile reuses the compiled files in its cache, which were
        compiled its own way.  So Guidance has its own cache,
        `$XDG_CACHE_HOME/guidance`.  `main.c` points `XDG_CACHE_HOME`
        there while Guile starts and then restores it, so the program
        and GTK see the value it had.

      `(fib 30)` takes 750 ms, against 710 ms before.  Test: one in
      `tests/repl-test.scm`, with `tests/programs/names.scm`.  The
      test script's module is now not declarative, as the module
      that Guidance runs programs in is, because that changes how
      they are compiled.

      Still open: a variable bound to the result of a call is not
      listed until one step after the line that binds it.  Guile
      counts the instruction that stores it as part of the next
      line.
- [x] **Procedure breakpoints stop working when the program is run
      again.**  A trap made from the Modules page or with
      `add-trap-at-procedure-call!` belonged to the procedure object.
      Running the program again redefines its procedures, and the
      trap, still listed as active on the Traps page, never fired
      again.  Fixed: these traps now belong to the binding, the
      module's variable, and trap calls to whichever procedure it
      holds at the time (`gdn-trap-at-binding` in `(gdn lib)`).  The
      Modules page makes them directly.  `(gdn repl)` replaces
      `add-trap-at-procedure-call!` for the operator: it makes one
      when the procedure is a binding of its own name, in the current
      module or any loaded one, and otherwise falls back to Guile's
      trap.  They are named "Breakpoint at NAME" rather than
      "Breakpoint at #<procedure NAME (x)>".  Tests: three in
      `tests/repl-test.scm`, with `tests/programs/greet.scm`.

      Still open: a procedure that is not bound under its own name,
      such as an anonymous one or one reached only through an alias
      from the REPL, gets Guile's trap, which does not survive being
      defined again.
- [x] **Stepping leaves the program.**  Step Out of a top-level form
      paused in `boot-9.scm:4388`, inside `load`, and further steps
      stayed in Guile's internals.  Step at the last line of the
      program gave "Step to an anonymous procedure", twice: Guidance's
      own frames, which had no file name because its modules are
      compiled from strings.  Instruction steps went on out of the
      program until they failed with the `make-stack` error of the
      first item.  Fixed:
      - Guidance's modules are compiled with the file names
        `gdn/lib.scm` and `gdn/repl.scm`, so `guidance-frame?`
        recognizes them, and no step pauses in them.
      - Step Out only pauses in the caller if it is the program's.
        If the caller is in Guile's libraries, as for a procedure
        called by `map`, or called last in its file and so by `load`,
        the return is reported and the program pauses at the next
        line of its own that it reaches.
      - Instruction steps go no further out than the outermost frame
        of the program.
      - A step that leaves the program lets it finish.

      Tests: five in `tests/repl-test.scm`, with
      `tests/programs/map.scm`, and one in `tests/gui-smoke.sh`.

      Still open: after instruction steps leave a file given with
      `-l`, they may not pause in the next file.  Line steps do.
- [x] **The menu button does nothing.**  It was a plain `GtkButton`.
      `menus.ui` was not used, and named actions that do not exist.
      There was no About dialog and no Quit, and the program could
      only be named on the command line, so Guidance started from its
      desktop entry was a REPL with nothing to run.  Fixed: the
      button is a `GtkMenuButton` with this menu:
      - Open Program (Ctrl+O): a file dialog, starting in the
        directory Guidance was started in.  The chosen file replaces
        the program from the command line, without arguments; the
        other options stay.  It is loaded on the Source page, so that
        breakpoints can be set before it runs.  Available at the
        `repl>` prompt only.
      - Keyboard Shortcuts (Ctrl+?): the existing overlay, which now
        lists the new shortcuts.
      - About Guidance: name, version, licence, website, author, and
        the Guile and GTK versions.  The icon is now also in the
        resources, so that it shows when Guidance is not installed.
      - Quit (Ctrl+Q): as closing the window.

      `menus.ui` is removed.  Tests: Open Program's effect in
      `tests/repl-test.scm`, Quit in `tests/gui-smoke.sh`.  The file
      dialog and the About dialog were checked by hand, with the
      mouse, and are not in the tests.

      Still open: arguments for a program chosen from the menu; a
      list of recent programs; the window title does not name the
      program.

- [x] **Step Into can corrupt the program, or crash Guidance.**
      Step Into (F11), pressed repeatedly through a line that throws
      and catches an exception, or that calls `load`, made the program
      go wrong: a `catch` returned `#<unspecified>`, errors appeared
      that the program does not have, or Guidance crashed.  The
      instruction-step buttons crashed Guidance every time on the same
      code.  There were four causes, all fixed:
      - **Debugging lookups in the instruction hook.**  Calling
        `frame-source` or `frame-procedure-name` from an instruction
        hook while Guile raises or catches an exception corrupts the
        VM; this was reproduced in plain Guile, without Guidance.  The
        apply, return and abort hooks are safe.  Steps now look up the
        stepped frame's code and current line while the program is
        paused, and their instruction hook only compares addresses
        (`add-step-trap!` in `(gdn repl)`).  Entering a procedure of
        the program (Step Into, a tail call), returning to the
        program, and an exception caught by the program are found with
        the other hooks.  Step Out's fallback, "pause at the next line
        of the program", uses only those hooks too.
      - **Callers shown as the executing frame.**  Pausing in a caller
        whose callee's frame is still on the stack (after a return,
        for Step Out, and for `_break_`) told Guile it was the
        innermost frame.  Guile then listed the values the pending
        call would define, whose slots hold garbage, and printing one
        could crash.  These pauses now say it is not the top frame.
      - **Bindings beyond the frame.**  At some instructions a frame
        has bindings past its last slot; reading one reads past the
        frame.  Guidance now skips them, as Guile's own
        `frame-call-representation` does (`gdn-binding-readable?`).
      - **Objects half built.**  Between the instruction that
        allocates a pair and those that fill it in, showing the frame
        printed an invalid object.  So the toolbar's instruction steps
        now step by expression: to the next source position, line and
        column, where the objects of earlier expressions are complete.

      Also, building a backtrace entry can no longer raise an error,
      and `pause` reports a failure to update the pages rather than
      let it unwind through the VM hook; a rest argument that is not
      available no longer breaks the backtrace.  Tests: two in
      `tests/repl-test.scm`, each walking through
      `tests/programs/step-into.scm` three times; on the old code they
      crash the test run.

#### Should fix

- [x] **Other threads are outside the debugger.**  `_break_` in a
      thread made with `call-with-new-thread` was ignored without a
      word, and an error there printed Guile's own backtrace.  Fixed
      as far as an alpha needs: while a program runs, Guidance
      replaces `call-with-new-thread` in `(ice-9 threads)`, which the
      program's code and `begin-thread` refer to, and restores it
      afterwards.  An error that a thread does not handle is then
      reported in three lines, saying that the thread ended and that
      Guidance can pause only the main thread; a thread given a
      handler of its own keeps it.  `_break_` in another thread says
      that it was ignored.  The README lists the limit.  Test: one in
      `tests/repl-test.scm`, with `tests/programs/threads.scm`.

      Still open: debugging other threads; threads started through
      `make-thread`, futures, or `par-map`, which Guile's own code
      starts, keep Guile's messages.
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

#### Other concerns

Things noticed along the way that the items above do not cover.

- **Sanitizers.**  None of this phase's changes were run locally
  under ASan and UBSan; the GitHub workflow does that.  The GUI test
  now has fifteen scenarios and takes 35 to 45 seconds, so its
  time limit, and that of the REPL test, was raised from 120 to 300
  seconds for the sanitizer job.
- **The GUI test depends on timing.**  It waits for text in the
  transcript.  Two failures while writing it were the test's fault,
  not Guidance's: text from another stream landing in the middle of
  a line it counted, and a key sent the instant a message appeared.
  Both are dealt with, but the pattern invites more.  It also drives
  a window with no window manager, where only the window under the
  pointer gets keys, and it cannot work a file dialog.
- **The compile options are tuned to Guile 3.0.9.**  Which four
  optimizations to turn off was found by experiment.  Another
  version of Guile may rename variables in another pass.  The test
  in `tests/repl-test.scm` would catch that.
- **Libraries keep their optimized names.**  Only what Guidance
  compiles itself is compiled for debugging.  Modules that are
  installed already compiled, or found through `-C`, are used as
  they are, so their variables can still show as `tmp`.
- **Debugged code is not the code that ships.**  With four
  optimizations off, a program runs a little differently under
  Guidance than under Guile: slower, and without the inlining that
  could hide or cause a fault.
- **Guidance's cache is never cleaned.**  `~/.cache/guidance` grows
  as Guile's own cache does.
- **Standard input is non-blocking, and port procedures are
  replaced.**  `install-suspendable-ports!` changes `read-char`,
  `read-line`, `put-string` and the like for the whole process, to
  Guile's versions written in Scheme.  A program that reads file
  descriptor 0 itself, or passes it to a library, gets Guidance's
  own standard input, not the Terminal's; one that reads the port's
  descriptor without Guile's port procedures can see "would block".
- **The "documented API only" rule has exceptions now.**  The
  optimization keywords (`#:partial-eval?` and the rest) are listed
  by `guild compile -O help` but not in the manual.  The tests call
  `set-module-declarative?!`.  Guidance already used
  `vm-add-apply-hook!` and `module-submodules`, and now also the
  return and abort hooks, and `frame-num-locals`, which `(system vm
  frame)` does not export.
- **Programs run in a module that is not declarative.**  `main` sets
  `user-modules-declarative?` to false, with a comment that doubts
  it.  It is why procedures are not inlined across definitions and
  why procedure breakpoints by name work.  It deserves a decision
  and a line in the README.
- **Stop does not stop everything.**  It ends the main thread's run.
  Threads the program started keep running, and keep printing, into
  the next run.
- **`_break_` in a library frame.**  Fixed with Step Into: the
  caller of `_break_` is no longer shown as the innermost frame.
- **Pause in library code.**  Pause looks at most 100 frames out
  for a frame of the program, and otherwise says "The program could
  not be paused here."  Not tried against deep library recursion.

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
