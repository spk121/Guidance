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
    # Sanitizer reports go to file descriptor 2, which is the
    # Terminal's, so look in the transcript too.
    if grep -E "CRITICAL|ERROR|runtime error|AddressSanitizer" "$LOG" ||
       grep -E "runtime error|AddressSanitizer" "$TRANSCRIPT"; then
        fail "problems were logged"
    fi
    kill "$APP" 2>/dev/null
    wait "$APP" 2>/dev/null
}

finish () {
    kill "$APP" 2>/dev/null
    wait "$APP" 2>/dev/null
    # In a known failure, which runs in a subshell: see known_failure.
    [ -n "${KNOWN_FAILURE:-}" ] && exit "$1"
    echo "--- transcript ---"; cat "$TRANSCRIPT"
    echo "--- log ---"; cat "$LOG"
    rm -rf "$WORK"
    exit "$1"
}

fail () {
    echo "FAIL: $*"
    finish 1
}

# How long wait_for waits, in tenths of a second.
WAIT_TICKS=300

# known_failure PLAN-ITEM SCENARIO: runs the function SCENARIO, which
# tests a fault that is known and not yet fixed, described by PLAN-ITEM
# of PLAN.md.  That it fails is expected.  If it passes, the fault has
# been fixed, and the scenario should join the tests proper; that
# fails the run, so that it is noticed.  The scenario runs in a
# subshell, and waits less long for what it expects.
known_failure () {
    if (KNOWN_FAILURE=1; WAIT_TICKS=100; "$2") > "$WORK/known" 2>&1; then
        cat "$WORK/known"
        fail "known failure '$2' ($1) now passes: make it a test proper"
    fi
    grep -E "^(===|FAIL)" "$WORK/known"
    echo "xfail: $2 (PLAN.md: $1)"
}

# wait_for TEXT [COUNT]: waits until TEXT appears COUNT times.
wait_for () {
    count=${2:-1}
    i=0
    while [ "$(grep -cF -- "$1" "$TRANSCRIPT")" -lt "$count" ]; do
        kill -0 "$APP" 2>/dev/null || fail "Guidance exited while waiting for '$1'"
        i=$((i + 1))
        [ $i -gt "$WAIT_TICKS" ] && fail "timed out waiting for '$1'"
        sleep 0.1
    done
    echo "ok: $1"
}

# refute TEXT: fails if TEXT has appeared.
refute () {
    if grep -qF -- "$1" "$TRANSCRIPT"; then
        fail "unexpected '$1'"
    fi
    echo "ok: no $1"
}

# expect_count TEXT COUNT: fails unless TEXT has appeared COUNT times.
expect_count () {
    n=$(grep -cF -- "$1" "$TRANSCRIPT")
    [ "$n" -eq "$2" ] || fail "'$1' appeared $n times, not $2"
    echo "ok: $1 ($2 times)"
}

key () {
    xdotool key "$@"
}

type_line () {
    xdotool type --delay 20 "$1"
    xdotool key Return
}

# Guidance compiles its own Scheme modules into its cache the first
# time, and loads them from there afterwards.  (The REPL test checks
# that they compile without warnings.)
OLD_CACHE=${XDG_CACHE_HOME-}
export XDG_CACHE_HOME=$WORK/cache
start
for module in lib repl; do
    ls "$XDG_CACHE_HOME"/guidance/modules/*/*/gdn/$module.go >/dev/null 2>&1 ||
        fail "gdn/$module.go is not in the cache"
done
echo "ok: modules cached"
stop_app
start
stop_app
if [ -n "$OLD_CACHE" ]; then
    export XDG_CACHE_HOME=$OLD_CACHE
else
    unset XDG_CACHE_HOME
fi

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

# F9 on a line with a breakpoint deletes it.  The Source page learns
# of the breakpoint from the REPL, which marks it in the margin.
start "$PROGRAMS/break.scm"
key F5
wait_for "[prompt break>]"
sleep 0.5
key F9
wait_for "Trap 0: Breakpoint at break.scm:6"
sleep 0.5
key F9
key F5
wait_for "[prompt repl>]" 2
key F5
wait_for "Break at break.scm:6" 2
refute "Breakpoint at break.scm:6, at"
key shift+F5
wait_for "Stopped."
stop_app

# Output and error output appear in the order they were written, and
# escape sequences are not shown.
start "$PROGRAMS/interleave.scm"
key F5
wait_for "plain bold plain"
tr '\r\n' '..' < "$TRANSCRIPT" | sed 's/\.\.*/./g' |
    grep -q "out 1.err 1.out 2.err 2.out 3.err 3.out 4.err 4.out 5.err 5" ||
    fail "output and error output are out of order"
echo "ok: in order"
refute ";;; compiling"
stop_app

# Stepping out of the program, and past its last line, lets it finish
# rather than pausing in the code that runs it.
start "$PROGRAMS/break.scm"
key F5
wait_for "[prompt break>]"
key shift+F11
wait_for "Returned from numbers"
wait_for "[prompt repl>]" 2
key F5
wait_for "[prompt break>]" 2
key F10
wait_for "Step to break.scm:7"
key F10
wait_for "Step to break.scm:8"
key F10
wait_for "Step to break.scm:9"
key F10
wait_for "Step to break.scm:10"
key F10
wait_for "[prompt repl>]" 3
type_line "(+ 40 2)"
wait_for "→ 42"
refute "boot-9.scm"
refute "anonymous procedure"
stop_app

# A program that prints a great deal loses none of it, and the terminal
# keeps only the latest part.
# The first run compiles the program.  The compiler's notes come by the
# error stream, and can land in the middle of an output line, so the
# lines are counted in a second run, which has none.
start "$PROGRAMS/flood.scm"
key F5
wait_for "end of flood"
wait_for "[prompt repl>]" 2
stop_app
start "$PROGRAMS/flood.scm"
key F5
wait_for "end of flood"
wait_for "[prompt repl>]" 2
refute ";;;"
expect_count "line " 30000
wait_for "line 1 λ→日本語 äöü"
wait_for "line 30000 λ→日本語 äöü"
wait_for "[old text dropped]"
refute "■"
refute "�"
type_line "(+ 40 2)"
wait_for "→ 42"
stop_app

# Keys pressed in the wrong mode are not saved up for later.  Run
# pressed twice runs the program once, even after Stop.
start "$PROGRAMS/loop.scm"
xdotool key F5 F5 F5 F10
wait_for "spinning"
sleep 0.5
key F5
key F10
key shift+F5
wait_for "Stopped."
wait_for "[prompt repl>]" 2
sleep 1
expect_count "Running" 1
expect_count "spinning" 1
refute "[prompt step>]"
stop_app

# Run and Step pressed while the program runs do not carry it past its
# next break.
start "$PROGRAMS/late-break.scm"
key F5
wait_for "started"
key F5
key F10
key F11
wait_for "[prompt break>]"
sleep 1
refute "after the break"
refute "[prompt step>]"
key F5
wait_for "after the break"
wait_for "[prompt repl>]" 2
# Steps and Stop do nothing at the REPL prompt, and Run still works.
key F10
key shift+F5
key F5
wait_for "started" 2
key shift+F5
wait_for "Stopped."
stop_app

# Stop ends an endless expression entered while the program is paused,
# and leaves the program paused.
start "$PROGRAMS/break.scm"
key F5
wait_for "[prompt break>]"
type_line "(let loop () (loop))"
sleep 0.5
key shift+F5
wait_for "The expression was stopped."
type_line "(+ 40 2)"
wait_for "→ 42"
key F5
wait_for "four"
wait_for "[prompt repl>]" 2
refute "Stopped."
stop_app

# A program waiting for input can be paused, continued, and stopped.
start "$PROGRAMS/input.scm"
key F5
wait_for "What is your name?"
key F6
wait_for "Paused at input.scm:6"
key F5
sleep 0.5
key shift+F5
wait_for "Stopped."
wait_for "[prompt repl>]" 2
type_line "(+ 40 2)"
wait_for "→ 42"
refute "Hello"
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

# Recursion without end enters error mode, and Stop leaves it.
start "$PROGRAMS/deep.scm"
key F5
wait_for "Stack overflow: the program used more than"
wait_for "[prompt error>]"
key shift+F5
wait_for "Stopped."
wait_for "[prompt repl>]" 2
stop_app

# A subprocess's output and input are the Terminal's.
start "$PROGRAMS/subprocess.scm"
key F5
wait_for "output from a subprocess"
wait_for "errors from a subprocess"
wait_for "Type a line for a subprocess:"
type_line "hello"
wait_for "the subprocess read: hello"
wait_for "[prompt repl>]" 2
stop_app

# Ctrl+D ends a running program's input, after any text in the entry.
start "$PROGRAMS/eof.scm"
key F5
wait_for "Type lines, then Ctrl+D:"
type_line "first"
xdotool type --delay 20 "last"
key ctrl+d
wait_for 'end of input after ("first" "last")'
wait_for "[prompt repl>]" 2
stop_app

# An expression over several lines, and the history.
start
type_line "(define (twice x)"
wait_for "[prompt ...]"
type_line "  (* 2 x))"
wait_for "[prompt repl>]" 2
type_line "(twice 21)"
wait_for "→ 42"
key Up
key Return
wait_for "→ 42" 2
key Up Up Down
key Return
wait_for "→ 42" 3
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

# Guile's harmless options are accepted, and -e takes a module's
# procedure.
start --debug --no-auto-compile -L "$PROGRAMS/modules" \
      -e "(@ (guidance-test greeting) greet-arguments)" "$PROGRAMS/args.scm" one
wait_for "guidance: --no-auto-compile is ignored"
key F5
wait_for "(greet-arguments ("
refute "unrecognized option"
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

# The operator's expressions, and the program, are in (guile-user),
# not in a module of Guidance's.
start
type_line "(module-name (current-module))"
wait_for "→ (guile-user)"
stop_app

# So a program may use any names, also ones that Guidance uses inside.
start "$PROGRAMS/clash.scm"
key F5
wait_for "clash program done"
wait_for "[prompt repl>]" 2
key F5
wait_for "clash program done" 2
wait_for "[prompt repl>]" 3
refute "Guile has exited"
type_line "(+ 40 2)"
wait_for "→ 42"
stop_app

# A program that closes its output port does not close Guidance's: the
# error is shown, the REPL goes on, and the next run has a port again.
start "$PROGRAMS/close-port.scm"
key F5
wait_for "closing"
wait_for "[prompt error>]"
type_line "(+ 40 2)"
wait_for "→ 42"
key shift+F5
wait_for "Stopped."
wait_for "[prompt repl>]" 2
key F5
wait_for "closing" 2
wait_for "[prompt error>]" 2
key shift+F5
wait_for "Stopped." 2
refute "Guile has exited"
stop_app

# The values that reenter.scm displays: lines that are a number, or
# an unspecified value where one should be.
displayed_values () {
    tr -d '\r' < "$TRANSCRIPT" | grep -E '^([0-9]+|#<.*)$' | tr '\n' ' '
}

# step_to_end KEY TEXT: presses KEY at each pause until TEXT appears.
step_to_end () {
    i=0
    until grep -qF -- "$2" "$TRANSCRIPT"; do
        kill -0 "$APP" 2>/dev/null || fail "Guidance exited while stepping"
        i=$((i + 1))
        [ $i -gt 100 ] && fail "stepping did not reach '$2'"
        key "$1"
        sleep 0.2
    done
}

# Stepping through a line that calls a continuation again does not
# change the value it passes.
for step_key in F10 F11; do
    start "$PROGRAMS/reenter.scm"
    key F5
    wait_for "[prompt break>]"
    step_to_end $step_key "done"
    values=$(displayed_values)
    [ "$values" = "1 10 20 " ] || fail "displayed $values, not 1 10 20"
    echo "ok: 1 10 20 with $step_key"
    stop_app
done

################################################################
# Known failures: faults found by the review recorded in PLAN.md as
# Phase 6, and not yet fixed.  See known_failure.

# Error output of a subprocess started without a shell is shown.
subprocess_error_output () {
    start "$PROGRAMS/subprocess-error.scm"
    key F5
    wait_for "subprocess finished"
    wait_for "from-the-error-stream"
    stop_app
}
known_failure "Phase 6, Error output from system* and pipes is thrown away" \
              subprocess_error_output

# The program named on the command line is on the Source page from the
# start, so that a breakpoint can be set before the first run.
source_at_startup () {
    start "$PROGRAMS/break.scm"
    key F9
    wait_for "Breakpoint at break.scm:"
    stop_app
}
known_failure "Phase 6, The Source page is empty until the program first pauses" \
              source_at_startup

# Quit, from the menu or with Ctrl+Q, ends Guidance cleanly, also while
# a program is running.
start "$PROGRAMS/loop.scm"
key F5
wait_for "spinning"
key ctrl+q
i=0
while kill -0 "$APP" 2>/dev/null; do
    i=$((i + 1))
    [ $i -gt 100 ] && fail "Guidance did not quit"
    sleep 0.1
done
wait "$APP"
status=$?
[ "$status" -eq 0 ] || fail "Guidance quit with status $status"
if grep -E "CRITICAL|ERROR|runtime error|AddressSanitizer" "$LOG"; then
    fail "problems were logged"
fi
echo "ok: quit"

echo "PASS"
finish 0
