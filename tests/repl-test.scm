;;; Copyright 2026 Michael Gran
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; Tests of the Guidance REPL and debugger core, (gdn repl), run in
;;; plain Guile with the GUI replaced by the stubs in tests/stubs.
;;;
;;; Run with the debug VM engine, as Guidance does, e.g.
;;;   guile --debug -L src -L tests/stubs tests/repl-test.scm

(use-modules (gdn repl)
             (gdn gui)
             (gdn ports)
             (ice-9 threads)
             (srfi srfi-1)
             (ice-9 format))

(define *test-dir* (dirname (current-filename)))
(define (program name)
  (string-append *test-dir* "/programs/" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test helpers

(define *failures* 0)
(define *current-test* "")

(define (check description ok?)
  (format #t "  ~a ~a~%" (if ok? "PASS" "FAIL") description)
  (unless ok?
    (set! *failures* (1+ *failures*))))

(define-syntax-rule (define-test name body ...)
  (begin
    (set! *current-test* name)
    (format #t "~a~%" name)
    (catch #t
      (lambda () body ...)
      (lambda (key . args)
        (check (format #f "no unexpected exception (~a ~s)" key args) #f)))))

(define *output* "")
(define *events* '())

(define (run-repl script . program-args)
  "Runs the REPL with the operator input in SCRIPT, as if Guidance had
been started with PROGRAM-ARGS.  Returns the REPL's status, and sets
*output* and *events*."
  (set-program-arguments (cons "guidance" program-args))
  (clear-events!)
  (set-input-script! script)
  (let ((status #f)
        (port (open-output-string)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (parameterize ((current-output-port port)
                       (current-error-port port))
          (set! status (gdn-top-repl))))
      (lambda ()
        (set! *output* (get-output-string port))
        (set! *events* (events))
        (format #t "  --- output ---~%~a  --------------~%" *output*)))
    status))

(define (output-contains? str)
  (and (string-contains *output* str) #t))

(define (output-order? . strs)
  "True if the strings appear in the output in this order."
  (let loop ((strs strs) (start 0))
    (cond
     ((null? strs) #t)
     ((string-contains *output* (car strs) start)
      => (lambda (i) (loop (cdr strs) (+ i (string-length (car strs))))))
     (else #f))))

(define (modes)
  "The sequence of modes reported to the GUI."
  (filter-map (lambda (e) (and (eq? (car e) 'mode) (cadr e))) *events*))

(define (top-frame)
  "The innermost entry of the most recent backtrace, or #f."
  (let ((bt (last-backtrace)))
    (and bt (> (vector-length bt) 0) (vector-ref bt 0))))

(define (top-frame-call) (and=> (top-frame) (lambda (f) (vector-ref f 0))))
(define (top-frame-file) (and=> (top-frame) (lambda (f) (vector-ref f 1))))
(define (top-frame-line) (and=> (top-frame) (lambda (f) (vector-ref f 2))))
(define (top-frame-binding name)
  "The printed value of the binding NAME in the innermost frame."
  (and=> (top-frame)
         (lambda (f)
           (any (lambda (b) (and (string=? (vector-ref b 0) name)
                                 (vector-ref b 3)))
                (vector->list (vector-ref f 5))))))

(define (checkpoint description pred input)
  "A script element that checks PRED when reached, then sends INPUT."
  (lambda ()
    (check description (pred))
    input))

(define *repl-thread* (current-thread))

(define (after-delay seconds thunk)
  "Runs THUNK in another thread after SECONDS, then runs gdn-async-poll
in the REPL thread, as the GUI does while a program runs."
  (call-with-new-thread
   (lambda ()
     (usleep (inexact->exact (round (* seconds 1000000))))
     (thunk)
     (system-async-mark gdn-async-poll *repl-thread*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(define-test "eval: values, definitions, and errors in expressions"
  (let ((status
         (run-repl
          (list '(eval "(+ 1 2)")
                '(eval "(define x 10)")
                '(eval "(values (* x 2) 'two)")
                '(eval "(if)")
                '(eval "(+ 1")
                '(quit 7)))))
    (check "quit returns its status" (eqv? status 7))
    (check "prints the welcome" (output-contains? "Welcome to Guidance"))
    (check "prints a value" (output-contains? "→ 3"))
    (check "definitions persist and multiple values print"
           (output-order? "→ 20" "→ two"))
    (check "syntax and read errors do not enter error mode"
           (not (memq 'error (modes))))
    (check "shows the repl prompt" (string-contains (prompts) "repl>"))))

(define-test "eval: an error enters error mode until Stop"
  (run-repl
   (list '(eval "(car 5)")
         (checkpoint "backtrace is shown on error"
                     (lambda () (last-backtrace))
                     '(eval "(* 6 7)"))
         '(stop #f)
         '(eval "(+ 1 1)")
         '(quit 0)))
  (check "reports the error" (output-contains? "Wrong type argument"))
  (check "evaluates in error mode" (output-contains? "→ 42"))
  (check "stop returns to the REPL"
         (output-order? "Stopped." "→ 2"))
  (check "modes go run, error, then back to repl"
         (equal? (take (modes) 4) '(repl run error repl))))

(define-test "run: error in a program shows the program's frames"
  (run-repl
   (list '(run #f)
         (checkpoint "innermost frame is (inner 5)"
                     (lambda () (equal? (top-frame-call) "(inner\n  5)"))
                     '(stop #f))
         '(quit 0))
   (program "error.scm"))
  (check "innermost frame is at error.scm line 2"
         (and (equal? (top-frame-line) 2)
              (string-suffix? "error.scm" (top-frame-file))))
  (check "backtrace has the program's frames, not Guile's"
         (let ((bt (last-backtrace)))
           (and (>= (vector-length bt) 2)
                (string-prefix? "(outer" (vector-ref (vector-ref bt 1) 0)))))
  (check "argument value is shown" (equal? (top-frame-binding "x") "5"))
  (check "the program stops at the error"
         (not (output-contains? "not reached"))))

(define-test "run: _break_ pauses, then step and continue"
  (run-repl
   (list '(run #f)
         (checkpoint "break pauses at the call to _break_ (line 6)"
                     (lambda () (equal? (top-frame-line) 6))
                     '(step #f))
         (checkpoint "step goes to line 7"
                     (lambda () (equal? (top-frame-line) 7))
                     '(step #f))
         (checkpoint "step goes to line 8"
                     (lambda () (equal? (top-frame-line) 8))
                     '(run #f))
         '(quit 0))
   (program "break.scm"))
  (check "output before and after the break"
         (output-order? "one" "two" "Break at break.scm:6" "three" "four"))
  (check "backtrace shows only the program's frames"
         (let ((bt (vector->list (last-backtrace))))
           (and (pair? bt)
                (every (lambda (f) (string-suffix? "break.scm" (vector-ref f 1)))
                       bt))))
  (check "enters trap mode" (memq 'trap (modes)))
  (check "shows the break prompt" (string-contains (prompts) "break>"))
  (check "returns to the repl afterwards" (eq? (last (modes)) 'repl)))

(define-test "run: Stop in trap mode abandons the program"
  (run-repl
   (list '(run #f)
         '(stop #f)
         '(quit 0))
   (program "break.scm"))
  (check "stops at the break" (output-order? "two" "Break at" "Stopped."))
  (check "the rest of the program does not run"
         (not (output-contains? "three"))))

(define-test "traps: procedure trap set from the REPL"
  (run-repl
   (list `(eval ,(format #f "(load ~s)" (program "procs.scm")))
         '(eval "(add-trap-at-procedure-call! square)")
         '(eval "(sum-of-squares 3 4)")
         (checkpoint "first trap is (square 3)"
                     (lambda () (equal? (top-frame-call) "(square\n  3)"))
                     '(continue #f))
         (checkpoint "second trap is (square 4)"
                     (lambda () (equal? (top-frame-call) "(square\n  4)"))
                     '(step-out #f))
         (checkpoint "step-out pauses in the caller"
                     (lambda () (string-prefix? "(sum-of-squares"
                                                (top-frame-call)))
                     '(continue #f))
         '(quit 0)))
  (check "step-out reports the return value"
         (output-contains? "Returned from square with 16"))
  (check "the expression finishes" (output-contains? "→ 25"))
  (check "trap mode is entered" (memq 'trap (modes))))

(define-test "traps: trap requested by the GUI while waiting"
  (run-repl
   (list `(eval ,(format #f "(load ~s)" (program "procs.scm")))
         (lambda ()
           (push-request! (list 'add-proc-trap
                                (module-ref (resolve-module '(guile-user))
                                            'square)))
           '(wake #f))
         '(eval "(square 9)")
         (checkpoint "trap fires on (square 9)"
                     (lambda () (equal? (top-frame-call) "(square\n  9)"))
                     '(continue #f))
         '(quit 0)))
  (check "the request is acknowledged" (output-contains? "Breakpoint at"))
  (check "the expression finishes" (output-contains? "→ 81")))

(define-test "run: Stop interrupts a running program"
  (run-repl
   (list (lambda ()
           (after-delay 0.5 request-stop!)
           '(run #f))
         '(quit 0))
   (program "loop.scm"))
  (check "the program started" (output-contains? "spinning"))
  (check "the program was stopped" (output-contains? "Stopped."))
  (check "back in the repl" (eq? (last (modes)) 'repl)))

(define-test "run: trap requested by the GUI while running"
  (run-repl
   (list (lambda ()
           (after-delay 0.5
                        (lambda ()
                          (push-request!
                           (list 'add-proc-trap
                                 (module-ref (resolve-module '(guile-user))
                                             'tick)))))
           '(run #f))
         (checkpoint "the trap fires in the running program"
                     (lambda () (string-prefix? "(tick" (top-frame-call)))
                     '(stop #f))
         '(quit 0))
   (program "loop.scm"))
  (check "the program was stopped" (output-contains? "Stopped.")))

(define-test "run: a program that exits returns to the REPL"
  (run-repl
   (list '(run #f)
         '(eval "(+ 2 2)")
         '(quit 0))
   (program "exit.scm"))
  (check "reports the exit status"
         (output-contains? "The program exited with status 3."))
  (check "the REPL keeps working" (output-contains? "→ 4")))

(define-test "run: no program given"
  (run-repl (list '(run #f) '(quit 0)))
  (check "says so" (output-contains? "No program was given")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format #t "~%~a failure~:p~%" *failures*)
(exit (if (zero? *failures*) 0 1))
