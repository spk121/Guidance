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

In terms of the coding philosophy.

- Generally follow GNOME coding style.
- Don't fight GTK4. Use GTK4's native widgets and use them in their
  common manner.
- Don't reverse engineer Guile. Use only documented Guile 3.x API.

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
