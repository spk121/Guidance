Guidance
========

Guidance is a graphical debugger for GNU Guile

At the moment, it is garbage code, and does not function.

Do not build.  
Do not view.  
Do not feel the need to tell me it is broken.

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

What this is
------------

Guidance is a version of the Guile REPL with a GUI and some extra
features.  Largely, you just call Guidance the way you would call
Guile.

Alternative REPL
-----------------

The read-eval-print-loop (REPL) that Guidance provides is different
from that provided by standard Guile: simpler and less flexible.  The
Guidance REPL has four distinct modes.

1. In the Eval Mode, the user can enter expressions into the single-
   line entry. Each entry is expected to be a complete expression.
   If the expression can be parsed, it is executed in Run Mode.
   
   Also, if the Run Button is clicked, the command-line arguments
   are parsed and excuted in Run Mode.
   
2. In Run Mode, the expression is run. Output to the current output
   port and the current error port is sent to the terminal
   widget. Input can be entered from the line-entry widget. This
   continues until the exepression returns, an error occurs, or a
   trap is hit.  An error changes to Error Mode. A trap changes to
   Trap Mode.
   
   During Run Mode, the Pause and Stop Buttons are enabled. The Pause
   Button asynchronously inserts a breakpoint. The Stop Button
   asynchronously inserts an execution termination.
   
3. In Error Mode, the backtrace is populated, and the Error REPL is
   enabled. The Error REPL is an interpreter in the stack context of
   the error. Unlike the Eval REPL, the Error REPL evaluates each
   entry as if in a 'false-if-exception' context.
   
   During Error Mode, only the Stop Button is enabled. It quits Error
   Mode and returns to Eval Mode.
   
4. In Trap Mode, the backtrace is populated, and the Trap REPL is
   enabled.  The Trap REPL is like the Error REPL in that it is in the
   stack context of the trap.
   
   During Trap Mode, all of the step and next buttons are enabled,
   which add ephemeral traps and continue execution.  The Run Button
   continues execution. The Stop button returns to Eval Mode.

The guided experience replaces the standard Guile REPL with graphical
elements. Using a GUI toolbar and menu, you can set breakpoints and
step through the program.

The Peek Log
------------

There is a Peek log. Any call to the `_pk_` commands will log their
output on the Peek tab.

Breakpoints
-----------

A new procedure `_break_` is provided in the top-level environment,
which you can add to the code being debugged to ensure a breakpoint is
triggered at the location.

Really, `_break_` is procedure of zero arguments that does nothing,
but, Guidance adds a breakpoint on this procedure at startup.


Getting Started
---------------

The trickiest part about getting started with Guidance is getting
setting up the traps and breaks at locations where you want the
program to pause for inspection.  There are a few ways to do this.

1. In the code to monitored, insert calls to the `_break_` procedure
   at locations of interest.
2. Add traps using `add-trap-at-procedure-call!` or
   `add-trap-at-source-location!`.
   
   For example, to pause at `number->locale-string` in `(ice-9 i18n)`,
   you call `add-trap-at-procedure-call!` like this
   
       (add-trap-at-procedure-call! (@ (ice-9 i18n) number->locale-string))
   
3. Add traps at procedures using the **module** page

Docker Container
----------------

If the CI/CD system is working, it is creating the `spk121/guidance`
Docker container.  You can pull down the Docker container with

    docker pull spk121/guidance

or

    podman pull docker.io/spk121/guidance

In the container, the executable is at `/app/bin/guidance`.

Running a Docker container that contains a GUI requires an invocation
like the following:

    docker run -v /tmp/.X11-unix:/tmp/.X11-unix \
        -e DISPLAY=$DISPLAY -h $HOSTNAME spk121/guidance /app/bin/guidance
