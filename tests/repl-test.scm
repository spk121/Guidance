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
             (gdn lib)
             (gdn gui)
             (gdn ports)
             (ice-9 threads)
             (system vm trap-state)
             (srfi srfi-1)
             (ice-9 format))

;; Guidance runs programs in a module that is not declarative, which
;; changes how they are compiled.  Do the same here.
(set-module-declarative?! (current-module) #f)

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
  ;; Traps belong to the thread, so remove those of earlier tests.
  (for-each delete-trap! (list-traps))
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

(define (count-occurrences str)
  "How many times STR appears in the output."
  (let loop ((start 0) (n 0))
    (let ((i (string-contains *output* str start)))
      (if i
          (loop (+ i (string-length str)) (1+ n))
          n))))

(define (count-pauses)
  "How many times the program paused in trap mode."
  (count (lambda (mode) (eq? mode 'trap)) (modes)))

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

(define (top-frame-binding-names)
  "The names of the bindings in the innermost frame."
  (and=> (top-frame)
         (lambda (f)
           (map (lambda (b) (vector-ref b 0))
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

;; Running a program again defines its procedures again.  A trap at one
;; of them must go on working.

(define (greet-trapped?)
  (equal? (top-frame-call) "(greet\n  world)"))

(define-test "traps: a trap from the Modules page survives running again"
  (run-repl
   (list '(run #f)
         (lambda ()
           (push-request! '(add-proc-trap-by-name "(guile-user)" "greet"))
           '(wake #f))
         '(run #f)
         (checkpoint "the trap fires in the second run"
                     greet-trapped?
                     '(continue #f))
         '(run #f)
         (checkpoint "the trap fires in the third run"
                     greet-trapped?
                     '(continue #f))
         (lambda ()
           (push-request! `(disable-trap ,(car (list-traps))))
           '(wake #f))
         '(run #f)                      ; the trap is off: no pause
         (lambda ()
           (push-request! `(enable-trap ,(car (list-traps))))
           '(wake #f))
         '(run #f)
         (checkpoint "the trap fires again once it is enabled"
                     greet-trapped?
                     '(continue #f))
         (lambda ()
           (push-request! `(delete-trap ,(car (list-traps))))
           '(wake #f))
         '(run #f)                      ; the trap is gone: no pause
         '(quit 0))
   (program "greet.scm"))
  (check "the trap is named after the procedure"
         (output-contains? "Breakpoint at greet"))
  (check "pauses in three of the six runs" (equal? (count-pauses) 3))
  (check "every run finishes"
         (equal? (count-occurrences "Hello, world") 6)))

(define-test "traps: a trap from the REPL survives running again"
  (run-repl
   (list '(run #f)
         '(eval "(add-trap-at-procedure-call! greet)")
         '(run #f)
         (checkpoint "the trap fires in the second run"
                     greet-trapped?
                     '(continue #f))
         '(run #f)
         (checkpoint "the trap fires in the third run"
                     greet-trapped?
                     '(continue #f))
         '(quit 0))
   (program "greet.scm"))
  (check "pauses in two of the three runs" (equal? (count-pauses) 2))
  (check "every run finishes"
         (equal? (count-occurrences "Hello, world") 3)))

(define-test "traps: procedures without a binding of their own can be trapped"
  (run-repl
   (list '(eval "(define triple (let ((f (lambda (x) (* 3 x)))) f))")
         '(eval "(add-trap-at-procedure-call! triple)")
         '(eval "(triple 5)")
         (checkpoint "the trap fires" (lambda () (pair? (list-traps)))
                     '(continue #f))
         ;; A procedure of another module, named as the README does.
         '(eval "(add-trap-at-procedure-call! (@ (srfi srfi-1) last))")
         '(eval "(last '(1 2 3))")
         (checkpoint "the trap in the other module fires"
                     (lambda () (string-prefix? "(last" (top-frame-call)))
                     '(continue #f))
         '(quit 0)))
  (check "both expressions finish" (output-order? "→ 15" "→ 3"))
  (check "pauses twice" (equal? (count-pauses) 2)))

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

(define-test "run: errors and _break_ in other threads are reported"
  (let ((original (@ (ice-9 threads) call-with-new-thread)))
    (run-repl (list '(run #f) '(quit 0)) (program "threads.scm"))
    (check "an error in a thread is reported in a few lines"
           (output-order? "An error in a thread of the program ended the thread"
                          "In procedure car"
                          "Guidance can pause and inspect only"
                          "joined the failed thread"))
    (check "without Guile's backtrace"
           (not (output-contains? "Backtrace:")))
    (check "_break_ in another thread is reported, and ignored"
           (output-order? "_break_ was called in a thread other"
                          "after _break_ in a thread"))
    (check "a thread's own handler is kept"
           (output-contains? "(handled oops)"))
    (check "the main thread is not paused"
           (not (or (memq 'trap (modes)) (memq 'error (modes)))))
    (check "the program finishes" (output-contains? "end"))
    (check "call-with-new-thread is Guile's again after the run"
           (eq? original (@ (ice-9 threads) call-with-new-thread)))))

(define-test "run: no program given"
  (run-repl (list '(run #f) '(quit 0)))
  (check "says so" (output-contains? "No program was given")))

(define (request input . request)
  "A script element that queues REQUEST, as the GUI does, then sends
INPUT."
  (lambda ()
    (push-request! request)
    input))

(define (load-expression name)
  `(eval ,(format #f "(load ~s)" (program name))))

(define-test "traps: breakpoints at source lines"
  (run-repl
   (list (load-expression "procs.scm")
         (request '(wake #f) 'add-source-trap (program "procs.scm") 2)
         (request '(wake #f) 'add-source-trap (program "procs.scm") 3)
         (request '(wake #f) 'add-source-trap (program "procs.scm") 5)
         '(eval "(sum-of-squares 3 4)")
         (checkpoint "pauses at line 5 first"
                     (lambda () (equal? (top-frame-line) 5))
                     '(continue #f))
         (checkpoint "then at line 2, in square"
                     (lambda () (and (equal? (top-frame-line) 2)
                                     (string-prefix? "(square" (top-frame-call))))
                     '(continue #f))
         (checkpoint "and at line 2 again, for the second call"
                     (lambda () (equal? (top-frame-line) 2))
                     '(continue #f))
         '(quit 0)))
  (check "reports the breakpoint"
         (output-contains? "Breakpoint at procs.scm:2"))
  (check "a line with no code is reported"
         (output-contains? "No code is loaded at procs.scm:3 yet"))
  (check "the expression finishes" (output-contains? "→ 25")))

(define-test "traps: a breakpoint set on the paused line"
  (run-repl
   (list '(run #f)
         (request '(continue #f) 'add-source-trap (program "break.scm") 6)
         '(run #f)
         (checkpoint "the new breakpoint fires on the next run"
                     (lambda () (equal? (top-frame-line) 6))
                     '(stop #f))
         '(quit 0))
   (program "break.scm"))
  (check "continuing does not stop again on the same line"
         (output-order? "Break at break.scm:6" "four"
                        "Breakpoint at break.scm:6, at break.scm:6")))

(define-test "traps: a breakpoint set before its code is loaded"
  (run-repl
   (list (request '(run #f) 'add-source-trap (program "pending.scm") 10)
         (checkpoint "the program breaks at line 8"
                     (lambda () (equal? (top-frame-line) 8))
                     '(continue #f))
         (checkpoint "then the breakpoint fires, once its code is loaded"
                     (lambda () (equal? (top-frame-line) 10))
                     '(stop #f))
         '(quit 0))
   (program "pending.scm"))
  (check "the breakpoint is pending at first"
         (output-contains? "No code is loaded at pending.scm:10 yet")))

(define-test "traps: disable, enable, and delete"
  (define (first-trap) (car (list-traps)))
  (run-repl
   (list (load-expression "procs.scm")
         (request '(wake #f) 'add-proc-trap-by-name "(guile-user)" "square")
         (lambda ()
           (push-request! (list 'disable-trap (first-trap)))
           '(eval "(square 2)"))
         (lambda ()
           (push-request! (list 'enable-trap (first-trap)))
           '(eval "(square 3)"))
         (checkpoint "an enabled trap fires"
                     (lambda () (equal? (top-frame-call) "(square\n  3)"))
                     '(continue #f))
         (lambda ()
           (push-request! (list 'delete-trap (first-trap)))
           '(eval "(square 4)"))
         (checkpoint "deleted traps are gone"
                     (lambda () (null? (list-traps)))
                     '(quit 0))))
  (check "a disabled trap does not fire" (output-contains? "→ 4"))
  (check "the trap fires once" (= 1 (count (lambda (e) (equal? e '(mode trap)))
                                           *events*)))
  (check "a deleted trap does not fire" (output-contains? "→ 16")))

(define-test "modules: list modules and their procedures"
  (run-repl
   (list (load-expression "procs.scm")
         (request '(wake #f) 'module-procedures "(guile-user)")
         '(quit 0)))
  (check "the module list is sent"
         (and (last-module-list)
              (member "(guile-user)"
                      (map (lambda (m) (vector-ref m 0))
                           (vector->list (last-module-list))))))
  (let* ((procs (or (module-procedures "(guile-user)") #()))
         (square (find (lambda (p) (string=? (vector-ref p 0) "square"))
                       (vector->list procs))))
    (check "procedures are listed with their source"
           (and square
                (string-suffix? "procs.scm" (vector-ref square 1))
                (= 1 (vector-ref square 2))
                (vector-ref square 3)))))

(define-test "pause: pause a running program, inspect, and continue"
  (run-repl
   (list (lambda ()
           (after-delay 0.5 request-pause!)
           '(run #f))
         (checkpoint "pauses in the program"
                     (lambda () (string-suffix? "loop.scm" (top-frame-file)))
                     ;; Which locals are available depends on where
                     ;; the interrupt lands, so don't rely on them.
                     '(eval "(procedure? tick)"))
         (lambda ()
           (after-delay 0.5 request-stop!)
           '(continue #f))
         '(quit 0))
   (program "loop.scm"))
  (check "reports the pause" (output-contains? "Paused at loop.scm"))
  (check "expressions can be evaluated while paused"
         (output-contains? "→ #t"))
  (check "continues, then stops"
         (output-order? "Paused at" "Stopped.")))

(define-test "eval: local variables of the paused frame"
  (run-repl
   (list '(run #f)
         '(eval "(+ sum product)")
         '(eval "(define from-break 'yes)")
         (request '(eval "(list 'outer)") 'select-frame 1)
         '(run #f)
         '(eval "from-break")
         '(quit 0))
   (program "locals.scm"))
  (check "locals are in scope" (output-contains? "→ 19"))
  (check "frames can be selected"
         (output-contains? "Expressions are now evaluated in frame 1"))
  (check "definitions are made at the top level"
         (output-contains? "→ yes"))
  (check "the program finishes" (output-contains? "(7 12)")))

(define-test "eval: local variables in error mode"
  (run-repl
   (list '(run #f)
         '(eval "(* x 10)")
         '(stop #f)
         '(quit 0))
   (program "error.scm"))
  (check "the erring frame's locals are in scope"
         (output-contains? "→ 50")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line and peeking

(define-test "command line: parsing"
  (define (parse . args) (gdn-parse-command-line args))
  (define (ref alist key) (assq-ref alist key))
  (let ((a (parse "-L" "d1" "-L" "d2" "-C" "c" "-l" "pre.scm" "-e" "main"
                  "prog.scm" "x" "-L" "y")))
    (check "-L and -C directories, in order"
           (and (equal? (ref a 'load-path) '("d1" "d2"))
                (equal? (ref a 'compiled-path) '("c"))))
    (check "-l files and the -e function"
           (and (equal? (ref a 'load-files) '("pre.scm"))
                (eq? (ref a 'entry) 'main)))
    (check "the first non-option is the program; the rest are its arguments"
           (and (equal? (ref a 'script) "prog.scm")
                (equal? (ref a 'arguments) '("x" "-L" "y")))))
  (check "-s names the program"
         (equal? (ref (parse "-s" "-weird.scm" "a") 'script) "-weird.scm"))
  (check "-- ends the options"
         (equal? (ref (parse "--" "-x.scm") 'script) "-x.scm"))
  (check "-c gives an expression"
         (let ((a (parse "-c" "(f)" "p")))
           (and (equal? (ref a 'expression) "(f)")
                (equal? (ref a 'arguments) '("p")))))
  (check "bad options are reported"
         (and (equal? (ref (parse "-x" "f.scm") 'errors)
                      '("unrecognized option -x"))
              (equal? (ref (parse "-L") 'errors)
                      '("option -L requires an argument")))))

(define-test "command line: running a program with options and arguments"
  (run-repl (list '(run #f) '(quit 0))
            "-L" (program "modules") "-l" (program "preload.scm")
            "-e" "main" (program "args.scm") "a" "b")
  (check "the welcome names the program"
         (output-contains? "Press Run (F5) to run args.scm."))
  (check "-l files are loaded first, and -L finds modules"
         (output-order? "preloaded" "args" "Hello, world"))
  (check "the program sees its own command line"
         (output-contains? "args.scm a b))"))
  (check "-e calls the function with the command line"
         (output-order? "Hello, world" "(main (" "args.scm a b))")))

(define-test "command line: -c and bad options"
  (run-repl (list '(run #f) '(quit 0))
            "-q" "-c" "(display (command-line))" "p")
  (check "the expression runs with its arguments"
         (output-contains? "(guidance p)"))
  (check "unrecognized options are reported"
         (output-contains? "guidance: unrecognized option -q")))

(define-test "peek: _pk_ logs on the Peek page"
  (run-repl (list '(run #f) '(eval "(_pk_ 'from-repl 7)") '(quit 0))
            (program "peek.scm"))
  (check "values pass through _pk_" (output-order? "42" "→ 7"))
  (check "entries show where _pk_ was called"
         (and (member "peek.scm:2: (double 21)" (peeks))
              (member "peek.scm:5: (42)" (peeks))))
  (check "entries from the REPL have no location"
         (member "(from-repl 7)" (peeks))))

;; A step that is still pending when its run ends must not fire in a
;; later run, where it would pause outside the program.

(define-test "step: a step ended by an error does not disturb the next run"
  (run-repl
   (list '(run #f)
         '(step #f)                     ; to line 8
         '(step #f)                     ; to line 9
         '(step #f)                     ; to line 10
         '(step #f)                     ; into the error
         (checkpoint "the error is shown, in bad (line 4)"
                     (lambda () (equal? (top-frame-line) 4))
                     '(stop #f))
         '(eval "(+ 40 2)")
         '(run #f)
         (checkpoint "the next run pauses at its break (line 7)"
                     (lambda () (equal? (top-frame-line) 7))
                     '(stop #f))
         '(quit 0))
   (program "step-error.scm"))
  (check "the expression after the error is evaluated"
         (output-contains? "→ 42"))
  (check "the program runs twice"
         (output-order? "Break at step-error.scm:7" "Stopped."
                        "Break at step-error.scm:7" "Stopped."))
  (check "no internal error" (not (output-contains? "make-stack"))))

(define-test "step: a step ended by Stop does not disturb the next run"
  (run-repl
   (list '(run #f)
         (lambda ()
           ;; The step runs the loop, which never returns; stop it.
           (after-delay 0.3 request-stop!)
           '(step-out #f))
         '(eval "(+ 40 2)")
         '(quit 0))
   "-c" "(define (spin) (_break_) (let loop () (loop))) (spin)")
  (check "the program is stopped" (output-order? "Break at" "Stopped."))
  (check "the expression after Stop is evaluated" (output-contains? "→ 42"))
  (check "no internal error" (not (output-contains? "make-stack"))))

;; Open Program, in the menu, changes the program that Run runs.

(define-test "open: a program chosen from the menu replaces the one given"
  (run-repl
   (list '(run #f)
         (lambda ()
           (push-request! `(set-program ,(program "greet.scm")))
           '(wake #f))
         '(run #f)
         '(quit 0))
   (program "procs.scm") "one" "two")
  (check "first the program from the command line, then the one chosen"
         (output-order? "Press Run (F5) to run procs.scm." "procs.scm"
                        "Press Run (F5) to run greet.scm."
                        "greet.scm" "Hello, world"))
  (check "the chosen program has no arguments"
         (equal? (command-line) (list (program "greet.scm")))))

;; Programs are compiled so that they can be debugged: variables keep
;; their names, and stepping follows the lines as written.

(define-test "names: variables keep their names, and steps follow the lines"
  (run-repl
   (list '(run #f)
         (checkpoint "breaks in work (line 8)"
                     (lambda () (equal? (top-frame-line) 8))
                     '(step #f))
         (checkpoint "steps to the let (line 9)"
                     (lambda () (equal? (top-frame-line) 9))
                     '(step-into #f))
         (checkpoint "steps into helper, which is not inlined (line 3)"
                     (lambda () (and (equal? (top-frame-line) 3)
                                     (string-prefix? "(helper" (top-frame-call))))
                     '(step-out #f))
         (checkpoint "steps out, back to the let (line 9)"
                     (lambda () (equal? (top-frame-line) 9))
                     '(step #f))
         ;; a is bound by the first instruction of line 10, which has
         ;; not run yet.
         (checkpoint "steps to line 10"
                     (lambda () (equal? (top-frame-line) 10))
                     '(step #f))
         (checkpoint "steps to line 11, where a is 7"
                     (lambda () (and (equal? (top-frame-line) 11)
                                     (equal? (top-frame-binding "a") "7")))
                     '(step #f))
         (checkpoint "steps to line 12, where a is the only variable"
                     (lambda () (and (equal? (top-frame-line) 12)
                                     (equal? (top-frame-binding "a") "7")
                                     (equal? (top-frame-binding-names) '("a"))))
                     '(eval "(list 'a-is a)"))
         '(continue #f)
         ;; A procedure defined at the prompt keeps its names too.
         '(eval "(define (at-prompt n) (let ((m (* n 3))) (_break_) (+ m 1)))")
         '(eval "(at-prompt 4)")
         (checkpoint "m is 12 in the procedure from the prompt"
                     (lambda () (equal? (top-frame-binding "m") "12"))
                     '(eval "(list 'm-is m)"))
         '(continue #f)
         '(quit 0))
   (program "names.scm"))
  (check "a can be used in an expression" (output-contains? "→ (a-is 7)"))
  (check "m can be used in an expression" (output-contains? "→ (m-is 12)"))
  (check "the program finishes" (output-order? "14" "done" "→ 13")))

;; The operator must always be able to stop: an expression of their own
;; that does not finish, and a program that waits for input.

(define-test "stop: Stop ends an endless expression, not the paused program"
  (run-repl
   (list '(run #f)
         (lambda ()
           (after-delay 0.3 request-stop!)
           '(eval "(let loop () (loop))"))
         (checkpoint "the program is still paused at its break (line 6)"
                     (lambda () (equal? (top-frame-line) 6))
                     '(eval "(+ 40 2)"))
         '(continue #f)
         '(quit 0))
   (program "break.scm"))
  (check "the expression is stopped, later ones work, the program goes on"
         (output-order? "Break at break.scm:6" "The expression was stopped."
                        "→ 42" "three" "four"))
  (check "the program is not stopped" (not (output-contains? "Stopped."))))

(define-test "stop: Stop ends an endless expression in error mode"
  (run-repl
   (list '(run #f)
         (lambda ()
           (after-delay 0.3 request-stop!)
           '(eval "(let loop () (loop))"))
         '(eval "(+ 40 2)")
         '(stop #f)
         '(quit 0))
   (program "error.scm"))
  (check "the expression is stopped, then the program"
         (output-order? "Entering error mode" "The expression was stopped."
                        "→ 42" "Stopped.")))

(define-test "stop: Stop while the program waits for input"
  (run-repl
   (list (lambda ()
           (after-delay 0.5 request-stop!)
           '(run #f))
         '(eval "(+ 40 2)")
         '(quit 0))
   (program "input.scm"))
  (check "the program is stopped while it waits"
         (output-order? "What is your name?" "Stopped." "→ 42"))
  (check "it read nothing" (not (output-contains? "Hello"))))

(define-test "stop: Pause while the program waits for input, then input"
  (run-repl
   (list (lambda ()
           (after-delay 0.5 request-pause!)
           '(run #f))
         (lambda ()
           ;; Paused.  Once it continues, give it its input.
           (after-delay 0.3 (lambda () (send-input "Ünïcode\n")))
           '(continue #f))
         '(quit 0))
   (program "input.scm"))
  (check "pauses where it waits, then reads the input"
         (output-order? "What is your name?" "Paused at input.scm:6"
                        "Hello, Ünïcode!"))
  (check "pauses once" (equal? (count-pauses) 1)))

;; Stepping pauses only in the program.  When a step would take it into
;; the code that runs the program, the program runs on instead.

(define (stays-in-program?)
  (not (or (output-contains? "boot-9.scm")
           (output-contains? "gdn/")
           (output-contains? "anonymous procedure")
           (output-contains? "make-stack"))))

(define-test "step: step-out of the last call in a file ends the program"
  ;; (numbers) is in tail position, so its caller is `load'.
  (run-repl
   (list '(run #f)
         '(step-out #f)
         '(eval "(+ 40 2)")
         '(quit 0))
   (program "break.scm"))
  (check "the return is reported, the program ends, the REPL goes on"
         (output-order? "Break at break.scm:6" "four"
                        "Returned from numbers" "→ 42"))
  (check "no pause after the break" (equal? (count-pauses) 1))
  (check "never pauses outside the program" (stays-in-program?)))

(define-test "step: stepping past the last line ends the program"
  (run-repl
   (list '(run #f)
         '(step #f)                     ; to line 7
         '(step #f)                     ; to line 8
         '(step #f)                     ; to line 9
         '(step #f)                     ; to line 10
         (checkpoint "at the last line of numbers (line 10)"
                     (lambda () (equal? (top-frame-line) 10))
                     '(step #f))
         '(eval "(+ 40 2)")
         '(quit 0))
   (program "break.scm"))
  (check "the program ends, the REPL goes on" (output-order? "four" "→ 42"))
  (check "pauses at the break and four steps" (equal? (count-pauses) 5))
  (check "never pauses outside the program" (stays-in-program?)))

(define-test "step: step-into past the last line ends the program"
  (run-repl
   (list '(run #f)
         '(step-into #f) '(step-into #f) '(step-into #f) '(step-into #f)
         (checkpoint "at the last line of numbers (line 10)"
                     (lambda () (equal? (top-frame-line) 10))
                     '(step-into #f))
         '(eval "(+ 40 2)")
         '(quit 0))
   (program "break.scm"))
  (check "the program ends, the REPL goes on" (output-order? "four" "→ 42"))
  (check "pauses at the break and four steps" (equal? (count-pauses) 5))
  (check "never pauses outside the program" (stays-in-program?)))

(define-test "step: instruction steps past the end end the program"
  (run-repl
   (append (list '(run #f))
           ;; Far more than the program has left.  Those that remain
           ;; when it ends are ignored by the REPL.
           (make-list 300 '(step-instruction #f))
           (list '(eval "(+ 40 2)")
                 '(quit 0)))
   (program "break.scm"))
  (check "steps through the rest of numbers"
         (output-order? "Break at break.scm:6" "Step to break.scm:7"
                        "Step to break.scm:10"))
  (check "the program ends, the REPL goes on" (output-order? "four" "→ 42"))
  (check "never pauses outside the program" (stays-in-program?)))

(define-test "step: step-out to a library caller pauses at the next program line"
  ;; visit is called by map.
  (run-repl
   (list '(run #f)
         (checkpoint "breaks in the first call of visit (line 8)"
                     (lambda () (equal? (top-frame-line) 8))
                     '(step-out #f))
         (checkpoint "pauses on entering the second call of visit (line 5)"
                     (lambda () (and (equal? (top-frame-line) 5)
                                     (string-prefix? "(visit" (top-frame-call))))
                     '(step-out #f))
         (checkpoint "pauses back in visit-all, after map (line 12)"
                     (lambda () (and (equal? (top-frame-line) 12)
                                     (string-prefix? "(visit-all"
                                                     (top-frame-call))))
                     '(step-out #f))
         (checkpoint "pauses at the top level, where it was called (line 16)"
                     (lambda () (equal? (top-frame-line) 16))
                     '(continue #f))
         '(quit 0))
   (program "map.scm"))
  (check "both returns from visit are reported"
         (output-order? "Returned from visit with 2"
                        "Returned from visit with 4"
                        "(2 4)" "Returned from visit-all" "end"))
  (check "never pauses outside the program" (stays-in-program?)))

(define (last-mode)
  (let loop ((events (reverse (events))))
    (cond
     ((null? events) #f)
     ((eq? (car (car events)) 'mode) (cadr (car events)))
     (else (loop (cdr events))))))

(define (step-until-done action)
  "Script elements that send ACTION while the program is paused, Stop
in error mode, and quit once the REPL is back at its prompt."
  (map (lambda (i)
         (lambda ()
           (case (last-mode)
             ((repl) '(quit 0))
             ((error) '(stop #f))
             (else (list action #f)))))
       (iota 400)))

(define (step-through-catch-and-load action)
  ;; The bug this guards against did not happen every time.
  (do ((i 0 (1+ i))) ((= i 3))
    (run-repl (cons '(run #f) (step-until-done action))
              (program "step-into.scm"))
    (check (format #f "run ~a: the caught exception's value is right" i)
           (output-contains? "(k 1)"))
    (check (format #f "run ~a: the loaded file ran" i)
           (output-order? "(other 42)" "done"))
    (check (format #f "run ~a: no error appears" i)
           (not (memq 'error (modes))))))

(define-test "step: Step Into through catch and load keeps the program right"
  (step-through-catch-and-load 'step-into)
  (check "pauses in the procedures that catch calls"
         (>= (count-occurrences "Step to step-into.scm:5") 2)))

(define-test "step: expression steps through catch and load"
  ;; These used to stop at each instruction, which could show an
  ;; object half built, and crash.
  (step-through-catch-and-load 'step-into-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format #t "~%~a failure~:p~%" *failures*)
(exit (if (zero? *failures*) 0 1))
