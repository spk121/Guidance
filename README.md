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

Classic Experience
------------------

Classic mode brings up the tried-and-true Guile REPL -- full featured
and powerful -- with a few quality-of-life modifications.

- Classic mode always starts at a Guile REPL.  Even when called with
  command-line arguments that would normally cause a program to start
  executing, we start with a REPL.  To actually execute the
  command-line arguments, use the new meta-command `,run`.

- The `,backtrace` command as well as the family of `,step` and
  `,next` commands will update the *Backtrace* and *Source* tabs,
  providing a quicker experience to see context information and the
  stack.

Guided Experience
-----------------

The guided experience replaces the standard Guile REPL with graphical
elements. Using a GUI toolbar and menu, you can set breakpoints and
step through the program.

The Peek Log
------------

In both modes, there is a Peek log. Any call to the `pk` or `peek`
commands will log their output on the Peek tab.

Breakpoints
-----------

A new procedure `breakpoint` is provided in the top-level
environment, which you can add to the code being debugged to ensure
a breakpoint is triggered at the location.

Really, `breakpoint` is procedure of zero arguments that does nothing,
but, Guidance adds a breakpoint on this procedure at startup.



The extra features are these:


- When the REPL is prompting at a location with an associated debug
  frame, the *Source* and *Backtrace* tabs are updated to give
  context.
  
- The *Traps* tab provides an alternate way to see, disable, and
  enable traps.
  
- The *Peek* tab keeps a history of all calls to the `pk` macro and
  their output.
  
- 
  
- A simpler trap handler is available, and installed by default, so
  that when a trap is reached, the *Source* and *Backtrace* tabs are
  updated, and a button bar is enabled.

- The REPL adds a couple of new metacommands

  `,run`: This processes the command-line arguments

  `,where`: If the REPL currently is associated with a debug frame,
  `,where` or `,w` loads the source file of a given frame and displays
  its location in the *Source* tab.
  
- Some existing metacommands are given extra features

  `,backtrace` or `,bt` updates the *Backtrace* tab with the current
  backtrace information.
  
  `,step`, `,next` and all other associated 

Getting Started
---------------

The trickiest part about getting started with Guidance is getting
setting up the traps and breaks at locations where you want the
program to pause for inspection.  There are a few ways to do this.

1. In the code to monitored, insert calls to the `(_break_)` procedure
   at locations of interest.
2. Add traps using `add-trap-at-procedure-call!` or
   `add-trap-at-source-location!`.
   
   For example, to pause at `number->locale-string` in `(ice-9 i18n)`,
   you call `add-trap-at-procedure-call!` like this
   
       (add-trap-at-procedure-call! (@ (ice-9 i18n) number->locale-string))
   
3. Add traps at procedures using the **module** page
4. Start running in a paused state, by using `step-into` procedure.



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
