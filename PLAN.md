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
  The compiled code is now kept in Guidance's cache (see the alpha
  bugs below), so only the first start pays for it.
- Tests: `meson test` runs `tests/repl-test.scm`, which tests the
  debugger core in plain Guile with the GUI stubbed out
  (`tests/stubs`), and `tests/gui-smoke.sh`, which drives the real app
  on Xvfb using a terminal transcript (`GUIDANCE_TRANSCRIPT`).  The
  smoke test is skipped if Xvfb or xdotool is missing.
- Output and error text once arrived through separate
  pseudoterminals, so their relative order in the terminal could vary.
  They now share one (see the alpha bugs below).

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
- Breakpoint lines are marked in the Source page's margin (see the
  alpha bugs below).  Nested procedures can have line breakpoints, but
  the Modules page lists only procedures bound at a module's top
  level.

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
- [x] **Runaway recursion never ends.**  `(define (deep n) (+ 1 (deep
      (+ n 1))))` reaches "stack overflow" in 13 s in plain Guile, and
      was still running after 150 s under Guidance: the apply hook of
      the `_break_` trap made deep recursion quadratic (see the next
      item).  Fixed: with that hook gone, and a limit of 256 MiB set
      with `call-with-stack-overflow-handler`, the program enters
      Error mode within a second or two.  Error mode cannot run inside
      the overflow handler, because Stop would unwind out of it while
      the stack is still deep, and Guile then puts the program's limit
      back and it overflows again.  So the handler shows the backtrace
      while the frames exist, then unwinds, and Error mode runs on a
      shallow stack, with expressions at the top level.  Tests: one in
      `tests/repl-test.scm` and one in `tests/gui-smoke.sh`, with
      `tests/programs/deep.scm`.  With a breakpoint set, the apply
      hook is back, and deep recursion is slow to reach the limit.
- [x] **Programs run about 25 times slower.**  `(fib 30)` took 31 ms
      in plain Guile and 750 ms under Guidance.  The cause was not the
      trace level, which costs little by itself, but the trap that
      Guidance set on `_break_` for every run: it uses the VM's apply
      hook, so Guile called a Scheme procedure on every call the
      program made.  Those calls allocate, and each collection scans
      the whole stack, so deep recursion became quadratic.  Fixed:
      `_break_` now pauses by itself, as Pause does, with tracing off
      while paused.  `(fib 30)` takes 20 ms under Guidance.

      Breakpoints at lines and procedures still use the apply hook:
      that is how they attach to code loaded later and survive
      reloading.  While one is set, `(fib 30)` takes 925 ms.  The
      README says so.  Test: one in `tests/repl-test.scm`, with
      `tests/programs/fib.scm`.
- [x] **Subprocess output misses the Terminal.**  Only Guile's ports
      were redirected, not file descriptors 0 to 2, so the output of
      `(system "echo hi")` went to the terminal Guidance was started
      from.  (`system*` and pipes already followed Guile's ports.)
      Fixed: file descriptors 0 to 2 are now the Terminal's
      pseudoterminals, each opened anew so that standard input stays
      blocking for subprocesses.  Guidance's own messages keep their
      destination: the C library's `stdout` and `stderr` use copies of
      the original descriptors.  Sanitizer reports, written to
      descriptor 2, now appear in the Terminal; the GUI test looks
      for them in its transcript too.  Test: one in
      `tests/gui-smoke.sh`, with `tests/programs/subprocess.scm`.
- [x] **No end-of-file for standard input.**  Ctrl+D in the entry did
      nothing, so a program that read until EOF could only be stopped.
      Fixed: while a program runs, Ctrl+D sends the text in the entry,
      if any, as a line, then end-of-file, using the pseudoterminal's
      EOF character in canonical mode.  The input pseudoterminal's
      echo is now off: the Terminal shows typed text itself, and the
      echo piled up unread.  The shortcuts overlay and README list
      Ctrl+D.  Test: one in `tests/gui-smoke.sh`, with
      `tests/programs/eof.scm`.
- [x] **The entry takes one line, and has no history.**  An unfinished
      expression was a read error, so nothing longer than a line could
      be entered; the history was filled but never read, its duplicate
      check compared only a prefix, and the button beside the entry was
      empty.  Fixed:
      - While what has been entered ends part-way through an
        expression, the REPL shows the prompt `...` and appends the next
        line (`get-user-input` in `(gdn repl)`), in every mode.  Any
        other action abandons the unfinished expression, and says so.
      - Up and Down go through the entries, as in a shell, and Down past
        the newest gives back what was being typed.  Empty entries and
        repeats of the newest are not kept; at most 500 are.
      - The button beside the entry lists the 15 most recent entries;
        choosing one puts it back in the entry.

      Tests: one in `tests/repl-test.scm`, one in `tests/gui-smoke.sh`.
- [x] **Backtrace page layout.**  At the default window size the Value
      column, the one that matters, is off the right edge, and the
      Location column is cut to "fact.sc".  The Type column is blank
      except for "A" on arguments.  A top-level frame is shown as
      `(_)`.  Fixed:
      - The variables columns are now Name, Value and Kind.  Value
        expands to take the free width and ellipsizes at the end, with
        the full text as a tooltip; any extra information Guile gives
        about the value is appended after "⇒".  The separate
        Representation and Info columns are gone.
      - Kind reads "argument" or "local", plus how the value is stored
        when it is not an ordinary Scheme object (for example
        "local, f64").
      - The Location label is wide enough for "program.scm:120" and,
        when it is not, drops the start of the name rather than the
        line number.  The stack pane starts wider (400 pixels) and the
        default window is 1000×650, so both panes fit without
        scrolling.
      - A procedure with no name (Guile names it `_`) is shown as `λ`,
        and the outermost frame, which is the top level of the program
        being loaded, as "(top level)".  The REPL test checks the
        latter.
      - Later, while taking the release screenshot: Kind called an
        argument "local" when its procedure had a closure, as every
        procedure does in Guidance's non-declarative `(guile-user)`.
        A binding was taken to be an argument if its index was below
        the number of arguments in the call, but the closure is
        binding 0.  Now the count comes from the procedure's arity
        (`argument-binding-count` in `(gdn lib)`): the closure, if
        any, then the required, optional, rest and keyword
        arguments.  The REPL test checks an argument and two locals
        (`tests/programs/inventory.scm`).
- [x] **Source page.**  No line numbers, and no mark on lines that
      have breakpoints (already noted under Phase 2).  Fixed:
      - The text view has a left margin (a `GtkDrawingArea` given to
        `gtk_text_view_set_gutter`) with the line numbers, dimmed, in
        the text's font, and a red disc on lines with a breakpoint, or
        a red ring when it is disabled.  It is redrawn as the text
        scrolls or is laid out.
      - The REPL keeps the trap index, file and line of each
        breakpoint at a line, and sends them with every update of the
        Traps page (`gdn-set-source-breakpoints!`), so the marks follow
        breakpoints set, disabled, or deleted from any page.  The
        REPL test checks what is sent.
      - Clicking a line in the margin sets a breakpoint there, or
        deletes the one there.  F9 and the context menu, now "Toggle
        Breakpoint", do the same for the cursor's line; the GUI test
        checks that F9 deletes.
      - Setting a breakpoint while paused took several seconds, which
        the marks made obvious: finding the code at a line decoded the
        line table of every loaded procedure, twice.  Each table is
        now decoded once and kept (`code-line-table` in `(gdn lib)`),
        so after the first breakpoint it takes a fraction of a second;
        line steps use the same tables.
- [x] **Threads page.**  Rows read `#<thread 139940890986176
      (7f46c934c6c0)>` with an emoji as ID.  Nothing says which is the
      program's thread, and Guidance's own threads are listed.  Fixed:
      - The REPL now makes the rows (`thread-rows` in `(gdn repl)`)
        rather than the C side listing `all-threads`.  The first is
        "Main thread", the program's, with the program's name and
        whether it is running, paused, or waiting at the REPL.
      - The replacement `call-with-new-thread` that Guidance installs
        during runs records each thread the program starts.  They are
        listed as "Thread 1", "Thread 2", ..., in the order they were
        started, with the name of the procedure each was started with
        (λ for an anonymous one), and "running" or "finished".  A new
        run forgets the finished ones, and numbers from 1 again when
        none is left.
      - Guidance's own threads and Guile's internal ones (the GTK
        thread, the finalizer, signal delivery) are no longer listed.
      - The emoji column, and `guidance-emoji.c` with it, are gone.
      - The REPL test checks the rows for a program with a finished
        thread and a running one.
- [x] **Compiler chatter and ordering in the Terminal.**  The first
      run of each file prints ";;; note: auto-compilation is enabled
      ...", ";;; compiling ..." and ";;; compiled ..." with long cache
      paths.  Because output and errors use separate pseudoterminals
      (noted under Phase 1), ";;; compiled" can arrive after "Break
      at", an error message after the `error>` prompt, and in one
      run a thread's backtrace landed in the middle of a line.  Hide
      the compiler notes, and merge the two streams in order.  Fixed:
      - The REPL's warning port is now a filter on the error port
        (`compiler-note-filter` in `(gdn repl)`) that drops the lines
        Guile prints when it compiles a file automatically: the
        auto-compilation note, ";;; compiling", ";;; compiled", and
        "source file ... newer than compiled".  It holds back a line
        only while it may still be one of those, that is, until its
        newline if it starts with ";;; ".  The compiler's warnings
        are kept.  The REPL test compiles a new file with an unbound
        variable, and checks.
      - Output and error output, from Guile's ports and from file
        descriptors 1 and 2, now go to one pseudoterminal, so the
        Terminal shows them in the order they were written.
      - To still show errors as such, Guile's error port is a port
        type of its own (`gdn-error-port`, in `guidance-lisp.c`) that
        writes each piece of text, in one write, between the ECMA-48
        codes for red and for the default color.  The Terminal takes
        red to mean error text, and drops the escape sequences, its
        own and any the program prints, keeping one that a read cuts
        short until the rest arrives.  So text that a program colors
        red is shown as error text too.
      - The GUI test runs a program that alternates output and error
        output, and checks the order, and that an escape sequence of
        the program's is not shown.
- [x] **Program output is not monospaced.**  Only echoed input and
      error text get the monospace tag.  Fixed: the whole Terminal is
      monospaced (the text view's `monospace` property, and GTK's
      `monospace` style class on the prompt and the entry), so
      columns line up in output, errors, and what is typed alike.
      Echoed entries, no longer set apart by their font, are bold.
      The text has a small margin from the frame.  There is no
      automatic test of fonts; checked by screenshot.
- [x] **Startup warnings.**  On Guile 3.0.9 Guidance prints
      "possibly unbound variable `total-processor-count'" and
      "`current-processor-count'" to its own stderr at every start,
      and the Environment page shows 0 for both; `(gdn lib)` needs
      `(ice-9 threads)`.  Startup takes 1.7 s, most of it compiling
      the Scheme modules (noted under Phase 1).  Fixed:
      - `(gdn lib)` imports the two procedures from `(ice-9 threads)`,
        so the warnings are gone and the Environment page shows the
        counts.  The REPL test compiles `(gdn lib)` and `(gdn repl)`
        and checks for warnings, and checks the counts.  (The GUI
        test cannot see these warnings: `(gdn lib)` is loaded before
        the standard ports are redirected, while Guile's error port
        still buffers for a file.)
      - Guidance's two modules are compiled once into
        `$XDG_CACHE_HOME/guidance/modules/VERSION/SHA256/gdn/`, keyed
        by the Guile version and a hash of the source, and loaded
        from there with `load-compiled` on later starts.  They are
        compiled with `compile-file` with that directory on the load
        path, so their procedures still record `gdn/lib.scm` and
        `gdn/repl.scm` as their files, by which the debugger knows
        Guidance's own frames.  If the cache cannot be written, they
        are compiled in memory as before.  Startup to the first
        prompt went from about 2.7 s to 0.6 s here.  The GUI test
        starts with an empty cache and checks that both are cached.
- [x] **Options.**  Guile options that Guidance does not know, such as
      `--no-auto-compile` and `--debug`, are reported as unrecognized
      after the first prompt.  Accept the harmless ones.  `-e` only
      takes a plain symbol, not Guile's `(@ (module) name)` form (from
      the code).  Fixed (`gdn-parse-command-line` in `(gdn lib)`):
      - `--debug`, `--no-debug`, `--auto-compile` and `-q` are
        accepted silently: Guidance always uses the debugging VM and
        its own compiler settings, and does not load `~/.guile`.
        `--no-auto-compile` is accepted with a note that it is
        ignored, since Guidance must compile the program to debug it.
      - Three more of Guile's options are supported, being simple:
        `-x EXTENSION` (added to `%load-extensions`),
        `--use-srfi=LIST` (the SRFIs are imported into
        `(guile-user)`), and `--fresh-auto-compile` (sets
        `%fresh-auto-compile`).  Others, such as `--listen`, are
        still reported as unrecognized.
      - `-e` reads an argument that starts with "(" as an expression,
        such as `(@ (my module) main)`, and evaluates it to find the
        procedure; one that cannot be read is reported.
      - `--help` lists the new options.  The REPL test checks the
        parsing and runs programs with `-e (@ ...)` and
        `--use-srfi`; the GUI test starts Guidance with `--debug
        --no-auto-compile` and `-e (@ ...)`.
- [x] **Leftovers.**  The last 340 lines of `src/gdn/lib.scm` are a
      commented-out copy of an old REPL.  `menus.ui` and the history
      fields are unused.  Tooltips are lower case and do not give the
      shortcuts ("run process" rather than "Run (F5)").  Fixed:
      - The commented-out REPL is gone from `(gdn lib)`, which now
        ends with the Environment page's procedures.
      - `menus.ui` and the old history fields had already gone; the
        Terminal's history is the one added for multi-line entry.
        The emoji helper went with the Threads page fix.  Also
        removed: the CSS rules for classes nothing uses (all but
        `fat_border`), the Terminal's two style classes that had no
        rules, and a commented-out debugging `printf` in
        `guidance-frame-info.c`.
      - The toolbar's tooltips are in title case and give their keys:
        "Run or Continue (F5)", "Stop (Shift+F5)", "Step Over (F10)",
        "Step Into (F11)", "Step Out (Shift+F11)"; the two expression
        steps have no keys.  Checked by screenshot; the existing tests
        cover the rest (the REPL test compiles `(gdn lib)` without
        warnings).
- [x] **Release housekeeping.**  The AppStream metadata lists release
      0.1.0 dated 2021-12-09 and has no screenshot.  There is no NEWS
      file.  Update the README's status paragraph and its list of
      limits to match what is decided above.  Done:
      - The version is 0.2.0, an alpha.  The AppStream metadata has a
        0.2.0 release dated 2026-11-22 (type "development") with a
        description, a longer description of Guidance, and a
        screenshot of the Backtrace page,
        `data/screenshots/backtrace.png`, referred to by its address
        on the master branch, which exists once this is merged.  It
        passes `appstream-util validate`, not only the relaxed check
        that `meson test` runs.
      - `NEWS` describes 0.2.0.
      - The README calls it an alpha, shows the screenshot, and points
        to NEWS.  Its known limitations add those noted below under
        "Other concerns" that a user would meet: libraries keep their
        optimized names, code runs a little differently with
        optimizations off, standard input is non-blocking, the
        Terminal is not a terminal emulator, and the cache is never
        cleaned.
      - The screenshot's program is `tests/programs/inventory.scm`.
        Taking it showed that Kind called arguments "local"; fixed in
        its own commit (see the Backtrace page item).

#### Other concerns

Things noticed along the way that the items above do not cover.

- **Sanitizers.**  None of this phase's changes were run locally
  under ASan and UBSan; the GitHub workflow does that.  (The fixes of
  the alpha bugs above were each run under both, locally, before they
  were committed.)  The GUI test
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

### Phase 6: Second review for the alpha

Review date: 2026-11-22, of `master` at 95655db (version 0.2.0), on
Ubuntu 24.04 with GTK 4.14.5 and Guile 3.0.9.  A clean build has no
warnings, and `meson test` passes 5 of 5, also in a build with ASan
and UBSan.  Guidance was then driven under Xvfb as in Phase 5, with
the earlier scenarios repeated and new ones for what has been added
since.  Everything below was reproduced in the running application.

Three problems are blockers.  The first is new since Phase 5, and
alone is reason not to release 0.2.0 as it is.

To be sure that none of this came from stale compiled files,
`~/.cache/guidance` and `~/.cache/guile` were then both deleted,
Guidance was built again from nothing, and every blocker and P1 item
was tried again.  All of them were still there.  The timings below
are from that second pass.

Each item now has a test, marked as a known failure: see "Tests for
these" at the end of this phase.

#### Blockers

- [x] **The program and the prompt run in Guidance's own module.**
      `(current-module)` at `repl>` is `(gdn repl)`, not
      `(guile-user)`, on every start.  So a program's definitions land
      among Guidance's, and replace them when the names match.  A
      program that defines procedures called `option`, `pause` and
      `program-name` runs once; the next Run ends the REPL thread with
      "Guile has exited.  Key: wrong-type-arg ...", and the window is
      then dead apart from closing it.  `(gdn repl)` has about 120
      such names, among them `pause`, `option`, `show-prompt`,
      `frame-line`, `eval-form`, and `refresh-views!`.

      It also empties the Modules page for the program: `(guile-user)`
      expands to nothing, because the program's procedures are in
      `(gdn repl)`, which the page hides.  Procedure breakpoints from
      that page are therefore not available for the program.

      Cause: since "cache Guidance's compiled modules", the modules
      are loaded with `load-compiled`, which leaves the current module
      set to the last module it defined, and the REPL thread inherits
      that.  The `set-current-module` in `spawn_top_repl` has never
      had an effect, because `scm_c_eval_string` restores the module
      when it returns; before the change nothing had moved it.  Fix:
      load the modules inside `save-module-excursion`, and set the
      module in `gdn-top-repl` itself.  Add a test that the prompt's
      module is `(guile-user)`, and one that runs a program whose
      names clash with Guidance's.

      Fixed: the compiled modules are loaded inside `save-module-
      excursion`, and `spawn_top_repl` sets the REPL thread's module to
      `(guile-user)` from C, where it stays set.  The two GUI tests, the
      prompt's module and a program with clashing names, are now
      ordinary tests.

- [x] **Stepping changes what the program computes, when a
      continuation is called.**  The fix for Step Into dealt with
      exceptions; full continuations still go wrong, and with Step
      Over as well.

          (define k #f)
          (define n 0)
          (define (go)
            (_break_)
            (display (call/cc (lambda (c) (set! k c) 1)))
            (newline)
            (set! n (1+ n))
            (when (< n 3) (k (* n 10)))
            (display "generator done"))
          (go)

      prints 1, 10, 20 in Guile, and in Guidance when continued from
      the break.  Stepped with F10 it printed 1, `#<unspecified>`, 20
      in three runs of three, and 1, `#<unspecified>`,
      `#<unspecified>` after the caches were cleared; with F11 it
      printed 1, 10, `#<unspecified>` once and the right values once.
      An escape is affected too: with F11,
      `(call/cc (lambda (k) (for-each (lambda (x) (when (> x 2) (k
      x))) '(1 2 3 4)) 'none))`, passed straight to a procedure, gives
      `#<unspecified>` instead of 3.  Nothing is reported; the program
      just carries on with the wrong value.

      Two things narrow it down.  It needs the continuation's value to
      go straight to a call, as in `(display (call/cc ...))`: with the
      value bound first, `(let ((v (call/cc ...))) (display v))`, the
      same program was right every time.  And it depends on the state
      of the process: in the GUI, where each try is a new process, F10
      went wrong every time, while in the REPL test, which runs many
      programs in one process, it went wrong once in six tries, on the
      first.

      Likely cause, not confirmed: `add-step-trap!` pauses from the
      abort hook (`(handler f #f)`), which Guile runs while the values
      being passed to the continuation are in transit; pausing runs a
      great deal of Scheme there.  The return hook may deserve the
      same suspicion.  A fix to try: in those hooks only note that
      the step has landed in that frame, and let the instruction hook
      pause at the next source position, where values are settled.

      A comparison of this kind should become a test: a program that
      exercises optional and keyword arguments, multiple values,
      records, parameters, fluids, `catch`, `dynamic-wind`, prompts,
      `call/cc`, `sort` and `fold` with the program's own procedures,
      and string ports, run once straight through and once under each
      kind of step, with the outputs compared.  One was written for
      this review; apart from the cases above, and the next item,
      Step Over and Step Into gave the same output as Guile for all of
      it.

      Fixed: the guess about the abort hook was half right.  With Step
      Over, the step paused from the instruction hook, at the first
      instruction run after the continuation returned into the stepped
      frame: in the middle of the expression that receives its value,
      before it was received.  A step now pauses only at an instruction
      where a source position begins.  With Step Into, the step paused
      from the abort hook, in the frame the continuation escaped to.
      That hook no longer pauses or looks anything up: the frames
      outside the stepped one are looked up beforehand, while the
      program is paused, and the hook only aims the step at the one that
      was landed in, where it pauses at the next source position.  In
      the GUI, `reenter.scm` is right six times of six with F10 and F11,
      and the comparison program now matches Guile under Step Into as
      well.  The GUI test and the two REPL tests are ordinary tests now.

- [x] **Guidance writes its own messages to the program's ports.**
      "Break at ...", "Step to ...", the values of the operator's
      expressions, "Stopped." and the rest go to whatever
      `(current-output-port)` is at the time, which the program
      controls.
      - Pause inside `with-output-to-string` or `with-output-to-file`,
        at a breakpoint or by stepping in, and the messages become
        part of the program's data.  A procedure that displays "A",
        calls `_break_`, and displays "B", with `(+ 1 2)` entered at
        the break, returned `"A\nBreak at wots.scm:3\n→ 3\nB"`, and
        wrote the same into its file.  Nothing shows in the Terminal
        meanwhile, so the pause looks like a hang.
      - A program that calls `set-current-output-port` leaves the
        Terminal silent for good: error mode's text, the results of
        expressions, and "Stopped." all vanish, also after the run.
      - A program that closes `(current-output-port)` ends the REPL
        thread, with a backtrace from `simple-format`, and the window
        is dead.

      Guidance should write to `%gdn-output-port` and
      `%gdn-error-port` explicitly, and give each run fresh bindings
      of the current ports, restored when it ends.  The operator's
      expressions while paused should print to the Terminal too.

      Fixed: Guidance has its own ports to the Terminal, which it writes
      all its messages to (`say` and `say-error` in `(gdn repl)`) and
      never hands out.  The program gets others: a duplicate of the
      output port, and a second error port from the new `gdn-open-error-
      port`.  They are made the current ports afresh for each run, so
      what a program does to the current ports ends with the run, and
      they are replaced if the program closed them.  An expression
      entered at a pause prints to the Terminal too.  The two REPL tests
      are now ordinary tests, with a third for expressions at a pause,
      and the GUI test has a program that closes its output port.  Still
      open: the program's standard input is shared, so a program that
      closes that is not protected.

#### P1: fix before the alpha if at all possible

- [x] **Stepping can take minutes, and looks like a hang.**  Stop
      does still work.  Two separate causes:
      - *Step Into through library code that makes many calls.*  The
        apply hook looks up debugging information for every procedure
        entered.  F11 through a `pretty-print` of 3,000 numbers took
        12.8 s, against 0.28 s with F10; F11 into a
        `(load "helper.scm")` of three lines took 62 s, and 135 s
        when the file also had to be compiled.  Remember the answer
        per code address, so that each procedure is looked up once.
      - *Step Over a line that recurses deeply.*  F10 over
        `(length (map (lambda (i) (number->string i)) (iota 30000)))`
        took 48 s, and 51 s after the caches were cleared, and with
        300,000 elements had not finished after two minutes; F10 over `(fib 25)`, a quarter of a million
        calls but never deep, took 1.1 s.  Guile's `map` is not
        tail-recursive, so the cost seems to grow with the depth of
        the stack while an instruction hook is installed.  Consider
        removing the instruction hook while the stepped frame is not
        the innermost, using the apply and return hooks to put it
        back.

      Fixed: measured in plain Guile, every call of a VM hook takes time
      in proportion to the depth of the stack, whichever hook it is, and
      the instruction hook is called most by far.  So a step now
      installs only the hooks it needs at the time: while a procedure
      that the stepped frame called is running, the instruction hook is
      removed, and so is the apply hook unless stepping into; the return
      hook puts them back.  And the apply hook remembers, per procedure,
      whether it is the program's (`entry-kind`).  In the GUI: Step Into
      through the `pretty-print` takes 0.18 s, was 12.8 s; into the
      `load`, 1.2 s, was 62 s; Step Over `(fib 25)` 0.17 s, was 1.1 s;
      over the `map` of 30,000, 2.3 s, was 48 s.  Both REPL tests are
      ordinary tests now.  Still open: the return hook remains while the
      callee runs, so the cost still grows with the square of the depth;
      Step Over the `map` of 300,000 elements still had not finished
      after 150 s.

- [x] **Error output from `system*` and pipes is thrown away.**  Since
      "show output and errors in order", `(current-error-port)` is a
      port of Guidance's own type, not a file port.  Guile's `system*`
      and `open-pipe` give a subprocess `/dev/null` for an error
      stream that is not a file port, so
      `(system* "sh" "-c" "echo oops >&2")` shows nothing, where plain
      Guile shows "oops".  `(system "...")`, which inherits file
      descriptor 2, is fine.  `(fileno (current-error-port))` now
      fails, which some programs use to test for a terminal.  The
      error port needs to be a file port again, or to have a
      descriptor.

      Fixed: the error port stays as it is, which keeps output and
      errors in order.  Instead `system*`, and `piped-process`, with
      which Guile's pipes are made, are wrapped: when the current error
      port is the program's Terminal port, they are given a file port
      for file descriptor 2, which is the Terminal's too.  If the
      program has sent its errors elsewhere, that is left alone.  The
      GUI test, now an ordinary test, covers `system*` and a pipe.
      Still open: `(fileno (current-error-port))` still fails, and a
      subprocess started some other way than these two primitives, with
      the current error port as its error stream, is not covered.

- [ ] **The Source page is empty until the program first pauses.**  A
      program named on the command line, the main way to start, is
      not shown, so no line breakpoint can be set before the first
      run without `_break_` or the Modules page (see the first
      blocker).  Open Program does load its file, and from there a
      breakpoint set before Run works.  Load the command-line program
      at startup the same way.
- [ ] **Stop and a second Run leave the program's threads running.**
      Noted in Phase 5 under "Other concerns".  With two worker
      threads, Stop left both printing into the Terminal, and running
      again made four.  At the least, say so when Stop is pressed and
      on the Threads page; better, end them, for example with
      `cancel-thread`, when a run ends.

#### P2: noticed, can wait

- Step Into on a line that uses `load` first pauses at
  "boot-9.scm:4419": the macro's expansion is part of the program's
  procedure but carries the library's file and line.  Steps should
  pass over positions in Guile's own files.
- "No code is loaded at spin.scm:3 yet" is printed for a breakpoint
  set, while paused, on a line of a procedure that is loaded, and
  that then fires at once.
- Up recalls lines, not entries: after an expression entered over
  two lines, Up gives its second line alone, which is a read error.
- A mistyped name at `repl>` enters Error mode, which then needs
  Stop.  This follows from running expressions as programs, but is
  heavy for a typo.
- File descriptors 1 and 2 are now the Terminal's, so what the C
  library, GLib's fallbacks, or a sanitizer writes straight to them
  appears in the Terminal, not where Guidance was started.  A
  sanitizer report in CI would be in the test's transcript, not its
  log, and lost if Guidance died first.
- Open Program shows the file with line 1 marked as the current
  line, though nothing is paused there.
- The breakpoint mark in the Source page's margin is a thin bar,
  easy to miss.

#### Tests for these

Each blocker and P1 item has a test that fails today.  So that the
suite still passes, and still says when something else breaks, they
are marked as known failures: a failing check is reported as XFAIL
and not counted.  When a fault is fixed, its test passes, which is
reported as XPASS and does fail the run, so that the test is moved
among the ordinary ones.  `define-known-failure` in
`tests/repl-test.scm` and `known_failure` in `tests/gui-smoke.sh` do
this; each names the item above that it belongs to.

| Item | Test | Where |
|------|------|-------|
| Guidance's own module | `prompt_module`, `clashing_names` | GUI |
| Continuations under steps | `step_over_continuation` | GUI |
| | "compare: a continuation called again", "compare: leaving a loop by a continuation" | REPL, intermittent |
| Messages to the program's ports | "ports: a pause while output goes to a string", "ports: a program that changes the output port" | REPL |
| Slow steps | "speed: Step Into a line that calls Guile's libraries a lot", "speed: Step Over a line that recurses deeply" | REPL |
| Error output of subprocesses | `subprocess_error_output` | GUI |
| Source page at startup | `source_at_startup` | GUI |
| Threads after Stop | "stop: Stop ends the threads the program started" | REPL |

Notes:

- The two REPL tests of continuations are marked intermittent
  (`define-intermittent-failure`): in one process the fault shows
  only now and then, so their passing is not news, and they have to
  be made ordinary tests by hand once it is fixed.  The GUI test has
  the fault every time.
- The module fault cannot be tested in the REPL test, which loads
  Guidance's modules from files and so never has it.
- The two speed tests give a step two seconds, then stop the program,
  so they cost four seconds while they fail.
- Not tested: a program that closes its output port, which would take
  the test's own port with it.
- Two ordinary tests were added as well, and pass: "compare: stepping
  does not change what a program prints", which runs
  `tests/programs/step-compare.scm`, 22 lines from as many features
  of the language, straight through and under each of the four kinds
  of step; and "speed: Step Over a line that calls Guile's libraries
  a lot".
- The GUI test takes about 85 s now, up from 45 s: each known failure
  waits up to ten seconds for what does not come.

#### Checked and working in this review

Startup in 0.4 s after the first start; `(fib 30)` in 15 ms with no
breakpoint set; runaway recursion stopped at the stack limit in 2 s,
with the stack shown; Step Into through `catch` and `throw`, four
runs of four, and under the sanitizers; errors and `_break_` in other
threads reported; the Threads page; `system` output and input for a
subprocess; Ctrl+D; expressions over several lines; Stop for an
endless expression while paused; Pause in library code, six of six;
breakpoints from the Source page's margin and F9, set while paused
and while running, and removed again; `-e (@ (module) name)`,
`--use-srfi`, `-x`, and Guile's ignored options; a directory or a
binary given as the program; a file with a byte that is not UTF-8;
a program that handles SIGINT itself; the history menu; Open
Program; `meson install`.
