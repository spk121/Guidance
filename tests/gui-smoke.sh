#!/bin/sh
# Copyright 2026 Michael Gran
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# A smoke test of the Guidance GUI.  It runs Guidance on a virtual X
# display, drives it with the keyboard, and checks the terminal
# transcript.  It is skipped (exit status 77) if Xvfb or xdotool is
# not installed.
#
# Usage: gui-smoke.sh GUIDANCE-BINARY TEST-PROGRAM-DIRECTORY
#
# Also checks `guidance --help' and `guidance --version'.

set -u
BIN=$1
PROGRAMS=$2

if ! command -v xdotool >/dev/null 2>&1; then
    echo "xdotool is not installed; skipping"
    exit 77
fi
if [ -z "${GUIDANCE_SMOKE_INNER:-}" ]; then
    if ! command -v xvfb-run >/dev/null 2>&1; then
        echo "xvfb-run is not installed; skipping"
        exit 77
    fi
    GUIDANCE_SMOKE_INNER=1
    export GUIDANCE_SMOKE_INNER
    exec xvfb-run -a -s "-screen 0 1024x768x24" "$0" "$@"
fi

"$BIN" --help | grep -q "^Usage: guidance" || { echo "FAIL: --help"; exit 1; }
"$BIN" --version | grep -q "^guidance " || { echo "FAIL: --version"; exit 1; }
echo "ok: --help and --version"

WORK=$(mktemp -d)
TRANSCRIPT=$WORK/transcript
LOG=$WORK/log
APP=

# start ARG...: starts Guidance with the command-line arguments ARG...,
# and a fresh transcript.
start () {
    : > "$TRANSCRIPT"
    : > "$LOG"
    echo "=== guidance $*"
    GUIDANCE_TRANSCRIPT=$TRANSCRIPT \
    GTK_A11Y=none \
    GDK_BACKEND=x11 \
    LC_ALL=C.UTF-8 \
    ASAN_OPTIONS=detect_leaks=0 \
        "$BIN" "$@" > "$LOG" 2>&1 &
    APP=$!
    wait_for "[prompt repl>]"
    # Without a window manager, keyboard input goes to the window
    # under the pointer.
    xdotool mousemove 200 200
}

# stop_app: checks that Guidance is healthy, then closes it.
stop_app () {
    kill -0 "$APP" 2>/dev/null || fail "Guidance exited"
    if grep -E "CRITICAL|ERROR|runtime error|AddressSanitizer" "$LOG"; then
        fail "problems were logged"
    fi
    kill "$APP" 2>/dev/null
    wait "$APP" 2>/dev/null
}

finish () {
    kill "$APP" 2>/dev/null
    wait "$APP" 2>/dev/null
    echo "--- transcript ---"; cat "$TRANSCRIPT"
    echo "--- log ---"; cat "$LOG"
    rm -rf "$WORK"
    exit "$1"
}

fail () {
    echo "FAIL: $*"
    finish 1
}

# wait_for TEXT [COUNT]: waits until TEXT appears COUNT times.
wait_for () {
    count=${2:-1}
    i=0
    while [ "$(grep -cF -- "$1" "$TRANSCRIPT")" -lt "$count" ]; do
        kill -0 "$APP" 2>/dev/null || fail "Guidance exited while waiting for '$1'"
        i=$((i + 1))
        [ $i -gt 300 ] && fail "timed out waiting for '$1'"
        sleep 0.1
    done
    echo "ok: $1"
}

key () {
    xdotool key "$@"
}

type_line () {
    xdotool type --delay 20 "$1"
    xdotool key Return
}

# Break, step, continue, then an error and Stop.
start "$PROGRAMS/break.scm"
key F5
wait_for "Break at break.scm:6"
wait_for "[prompt break>]"
key F10
wait_for "Step to break.scm:7"
key F5
wait_for "four"
wait_for "[prompt repl>]" 2

type_line "(car 5)"
wait_for "Entering error mode"
key shift+F5
wait_for "Stopped."
type_line "(+ 40 2)"
wait_for "→ 42"

stop_app

# A breakpoint set with F9 on the Source page, which follows the
# paused program.
start "$PROGRAMS/break.scm"
key F5
wait_for "Break at break.scm:6"
# The Source page loads the paused file on the GTK thread; let it.
wait_for "[prompt break>]"
sleep 0.5
key F9
wait_for "Breakpoint at break.scm:6"
key F5
wait_for "[prompt repl>]" 2
key F5
wait_for "Breakpoint at break.scm:6, at break.scm:6"
key shift+F5
wait_for "Stopped."
stop_app

# Pause a running program, evaluate an expression, then stop.
start "$PROGRAMS/loop.scm"
key F5
wait_for "spinning"
sleep 1
key F6
wait_for "Paused at loop.scm"
type_line "(procedure? spin)"
wait_for "→ #t"
key shift+F5
wait_for "Stopped."
stop_app

# _pk_ logs on the Peek page.
start "$PROGRAMS/peek.scm"
key F5
wait_for "[peek] peek.scm:2: (double 21)"
wait_for "[peek] peek.scm:5: (42)"
wait_for "[prompt repl>]" 2
stop_app

# While a program runs, the entry is its standard input.
start "$PROGRAMS/input.scm"
key F5
wait_for "What is your name?"
type_line "Guidance"
wait_for "Hello, Guidance!"
wait_for "[prompt repl>]" 2
stop_app

# Options and program arguments, as for Guile.
start -L "$PROGRAMS/modules" -e main "$PROGRAMS/args.scm" one two
wait_for "Press Run (F5) to run args.scm."
key F5
wait_for "Hello, world"
wait_for "args.scm one two))"
wait_for "(main ("
stop_app

# Stop interrupts a running program.
start "$PROGRAMS/loop.scm"
key F5
wait_for "spinning"
sleep 1
key shift+F5
wait_for "Stopped."
wait_for "[prompt repl>]" 2
type_line "(+ 1 1)"
wait_for "→ 2"
stop_app

echo "PASS"
finish 0
