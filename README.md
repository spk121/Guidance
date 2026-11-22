Guidance
========

Guidance is a graphical debugger for GNU Guile.

It is a version of the Guile REPL with a GUI and some extra features.
Largely, you call Guidance the way you would call Guile, then press
Run.

Guidance is under development.  The core debugger works: breakpoints,
stepping, pausing, and inspecting the stack and variables.  See
[PLAN.md](PLAN.md) for its status and what remains.

Philosophy
----------

There are a fairly powerful set of debugging tools in Guile and its
REPL, but, as far as I can tell, they are not often used.  For me, at
least, I avoid them because they are a bit cumbersome.

This project is an attempt to make different type of debugger that
suits my personal preferences: simpler and more graphical.

These are the self-imposed restrictions on this coding effort.

- Follow GNU coding style.
- Follow GNOME practices and styles.
- Don't fight GTK4. Use GTK4's native widgets and use them in their
  common manner.
- Use only Guile API that is documented in the reference manual, and
  use them in their common manner.

Building
--------

Guidance needs GTK 4.10 or later, GLib 2.74 or later, and Guile 3.0.9
or later, plus Meson and Ninja to build.

    meson setup _build
    ninja -C _build
    meson test -C _build

To build with the address and undefined-behavior sanitizers, add
`-Db_sanitize=address,undefined` to `meson setup`.  For the GCC static
analyzer, add `-Danalyzer=true`.

The tests need Guile.  The GUI test also needs Xvfb (`xvfb-run`) and
`xdotool`, and is skipped without them.  GitHub Actions builds
Guidance and runs the tests, with and without the sanitizers, on every
push and pull request; see `.github/workflows/build.yml`.

Running
-------

    guidance [OPTION]... [FILE [ARG]...]

Press Run (F5) to run FILE with the ARGs, as `guile FILE ARG...`
would.  The program sees its own command line, so `(command-line)`
returns `("FILE" "ARG" ...)`.  Without a FILE, Guidance is a REPL.

These options are a subset of Guile's:

| Option         | Meaning                                                    |
|----------------|------------------------------------------------------------|
| `-L DIRECTORY` | Add DIRECTORY to the front of the module load path.        |
| `-C DIRECTORY` | Like `-L`, but for compiled files.                         |
| `-l FILE`      | Load FILE before running the program.                      |
| `-s FILE`      | The program to run; the remaining arguments are its ARGs.  |
| `-c EXPR`      | The program to run is the expression EXPR.                 |
| `-e FUNCTION`  | After loading the program, call FUNCTION with its command line. |
| `--`           | Stop scanning options; the next argument is FILE.          |
| `-h`, `--help` | Display help and exit.                                     |
| `-v`, `--version` | Display version information and exit.                   |

Options end at FILE, so `guidance foo.scm --help` passes `--help` to
`foo.scm`.

Modes
-----

Guidance's REPL is in one of four modes.  The toolbar enables only the
buttons that make sense in the current one, and the terminal's prompt
shows which mode it is in.

1. **REPL** (`repl>`).  Each line entered is evaluated, and its values
   are printed.  Run (F5) runs the program from the command line.
   Expressions are run in the same way as a program, so errors and
   traps in them are handled as below.

2. **Run.**  The program is running.  Its output appears in the
   terminal, and what is typed in the entry is its standard input.
   Pause (F6) pauses it in Trap mode; Stop (Shift+F5) abandons it and
   returns to the REPL.

3. **Error** (`error>`).  The program raised an error.  The error is
   shown, and the Backtrace page shows the program's stack where the
   error happened.  Expressions can be evaluated to inspect it, with
   the local variables of the selected frame in scope.  Stop returns to
   the REPL.

4. **Trap** (`break>`, `trap N>`, `step>`, `pause>`).  The program is
   paused, at a breakpoint, after a step, or by Pause.  As in Error
   mode, the Backtrace page shows the stack and expressions can be
   evaluated in the selected frame.  Then:

   | Button   | Key       | Action                                       |
   |----------|-----------|----------------------------------------------|
   | Run      | F5        | Continue.                                    |
   | Step over | F10      | Run to the next line of this procedure, or of its caller. |
   | Step into | F11      | Run to the next line, including in procedures it calls. |
   | Step out | Shift+F11 | Run until this procedure returns, and show the values it returns. |
   | Stop     | Shift+F5  | Abandon the program and return to the REPL.  |

   Stepping skips over Guile's own libraries.  The toolbar also has
   buttons to step by machine instruction.

Pages
-----

- **Terminal**: the REPL, and the program's input and output.
- **Peek**: entries logged by `_pk_`; see below.
- **Backtrace**: the stack of the paused program, innermost first.
  Clicking a frame shows its variables, and makes it the frame in
  which expressions are evaluated.  Clicking a frame's location shows
  it on the Source page.
- **Source**: the source of the paused program, which follows it as it
  pauses.  "Set Breakpoint Here" in the context menu, or F9, sets a
  breakpoint at the cursor's line.
- **Traps**: the breakpoints and other traps.  Each can be switched off
  and on, or deleted.
- **Threads**: Guile's threads.
- **Modules**: the loaded modules.  Expanding one lists its procedures,
  each of which can be shown on the Source page or given a breakpoint.
- **Environment**: information about the process, such as environment
  variables, memory, and the locale.

Breakpoints
-----------

There are a few ways to pause a program where you want to look at it.

1. Call `_break_` in the program.  It is a procedure of no arguments
   that pauses the program at the line that calls it, when run by
   Guidance.  Elsewhere it does nothing.

2. Set a breakpoint at a line, from the Source page.  The breakpoint
   can be set before the program is loaded, and it remains when the
   program is run again.

3. Set a breakpoint at a procedure, from the Modules page.

4. Use Guile's trap procedures in the REPL, which Guidance makes
   available.  For example, to pause at `number->locale-string` in
   `(ice-9 i18n)`:

       (add-trap-at-procedure-call! (@ (ice-9 i18n) number->locale-string))

5. Press Pause (F6) while the program runs.

Breakpoints work with compiled code.  Programs run by Guidance are
compiled automatically, as Guile does by default.

The Peek log
------------

`_pk_` is like Guile's `pk`, for "peek": it returns its last argument,
and logs all of them, with the location of the call, on the Peek page
rather than the terminal.

    (define (double x)
      (_pk_ 'double x)
      (* 2 x))

logs `foo.scm:2: (double 21)` when called with 21.

Known limitations
-----------------

- Which local variables can be seen at a given point is up to Guile's
  compiler.  For example, a variable may be unavailable just after its
  last use.
- Expressions evaluated in a frame see copies of its variables;
  assigning to them does not change the program.
- Breakpoints at lines match exact lines.  A line with no code, such
  as a blank line, never pauses the program.
- The terminal shows the program's output and error output as they
  arrive, so their order can differ slightly from the order they were
  written in.
