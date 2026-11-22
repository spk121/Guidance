;;; (gdn repl)

;; Copyright (C) 2021  Michael L. Gran

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.


;;; This is a simplified REPL tailored for use with the Guidance
;;; GUI.  It runs in its own Guile thread.  The GUI talks to it
;;; through the primitives in (gdn gui):
;;;
;;; - gdn-get-user-input blocks until the operator does something,
;;;   and returns a list (ACTION DATA).  ACTION is one of the symbols
;;;   run, continue, stop, step, step-instruction, step-into,
;;;   step-into-instruction, step-out, eval (with DATA a string),
;;;   wake (a request is waiting), or quit.
;;;
;;; - gdn-take-requests! returns the requests, such as adding a trap,
;;;   that the GUI has queued since the last call.
;;;
;;; - While a program is running, the GUI calls gdn-async-poll
;;;   asynchronously in this thread, to deliver requests and to stop
;;;   the program.
;;;
;;; The REPL has four modes, which are reported to the GUI with
;;; gdn-set-input-mode!: repl, run, error, and trap.

;;; Goals
;;; - Use only functions documented in the reference manual
;;; - Be simple
(define-module (gdn repl)
  #:use-module (gdn lib)
  #:use-module (gdn gui)
  #:use-module (gdn ports)
  #:use-module (gdn widgets)
  #:use-module (ice-9 format)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm traps)
  #:use-module (system vm vm)
  #:use-module (system vm trap-state)
  #:use-module ((system vm trap-state)
                #:select ((add-trap-at-procedure-call!
                           . guile:add-trap-at-procedure-call!)))
  #:export (gdn-top-repl
            gdn-async-poll
            _break_
            _pk_)
  ;; Guidance's version is the one the operator gets; see below.
  #:replace (add-trap-at-procedure-call!)
  #:declarative? #f)

;; Aborting to this prompt stops the running program and returns to
;; the REPL.
(define %stop-tag (make-prompt-tag "gdn-stop"))

;; This prompt is installed directly around the program being run, so
;; that backtraces can be cut to show only the program's own frames.
(define %stack-tag (make-prompt-tag "gdn-stack"))

;; Aborting to this prompt stops an expression that the operator
;; entered while the program is paused, and returns to that pause.
(define %eval-tag (make-prompt-tag "gdn-eval"))

;; True while such an expression is being evaluated.
(define *in-eval* (make-parameter #f))

;; True while a program is running under the debugger.
(define *in-run* (make-parameter #f))

;; True while the program is paused in error mode or trap mode.
(define *paused* (make-parameter #f))

;; While paused, the frames of the program's stack, innermost first,
;; and whether the first is the frame that is executing.
(define *pause-frames* (make-parameter '()))
(define *pause-top-frame?* (make-parameter #f))

;; While paused, the index in *pause-frames* of the frame in which the
;; operator's expressions are evaluated.  The GUI selects it.
(define *selected-frame* 0)

;; The disable thunks of the one-shot traps, made by stepping, that
;; have not fired yet.  They belong to the run that made them.
(define *pending-steps* '())

;; The module list most recently sent to the GUI.
(define *last-module-list* #f)

;; Guidance's parsed command line; see gdn-parse-command-line.
(define *options* (gdn-parse-command-line '()))

(define (option key)
  (assq-ref *options* key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILING FOR DEBUGGING

;; Guile's optimizer makes a program hard to follow in a debugger.  A
;; `let' variable can turn up in a backtrace as `tmp', next to
;; temporaries of the compiler's own, and then cannot be used by name
;; in the operator's expressions; and procedures are inlined, so that
;; stepping jumps about in their lines.
;;
;; So Guidance compiles without the optimizations that do this: the
;; programs it loads, the modules they use, and the operator's
;; expressions.  The rest stay on.  The list was found by experiment,
;; with Guile 3.0.9, compiling into a module that is not declarative,
;; as the programs Guidance runs are:
;;
;; - partial evaluation inlines, and removes variables;
;; - simplification and dead-code elimination leave a variable that
;;   holds the result of a call named `tmp';
;; - common subexpression elimination adds variables named `box' and
;;   `arg'.
;;
;; Lower optimization levels keep the names too, but their line
;; information is worse: every statement of a body also counts as
;; being on the line where the body begins, so stepping keeps
;; returning there.
;;
;; main.c gives Guidance its own cache of compiled files, so that
;; files compiled by Guile itself are not used instead.
(define %debug-compile-options
  '(#:partial-eval? #f
    #:simplify? #f
    #:cse? #f
    #:eliminate-dead-code? #f))

(define (compile-for-debugging!)
  "Makes Guile compile the files that it loads from here on with
%debug-compile-options."
  (unless (memq #:partial-eval? %auto-compilation-options)
    (set! %auto-compilation-options
      (append %debug-compile-options %auto-compilation-options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS

(define (print-error-on-exception thunk)
  "Runs THUNK.  If an error occurs, an error message is displayed on
the current error port and *unspecified* is returned."
  (catch #t
    thunk
    (lambda (key . args)
      (print-exception-message (current-error-port) key args)
      *unspecified*)))

(define (print-exception-message port key args)
  "Displays a one-line description of an exception, which was thrown
with KEY and ARGS."
  (cond
   ;; (who message form subform)
   ((and (eq? key 'syntax-error) (= (length args) 4))
    (format port "Syntax error: ~a~@[ in ~s~]~%" (cadr args) (caddr args)))
   ;; The common case: (subr message format-args rest)
   ((and (= (length args) 4)
         (or (not (car args)) (symbol? (car args)) (string? (car args)))
         (string? (cadr args))
         (or (not (caddr args)) (list? (caddr args))))
    (apply display-error #f port args))
   (else
    (format port "Uncaught exception ~a: ~s~%" key args))))

(define (print-return-values . return-vals)
  "Prints the return values of an expression, which could be multiple
values."
  (for-each (lambda (val)
              (unless (unspecified? val)
                (display "→ ")
                (write val)
                (newline)))
            return-vals))

(define (read-all-from-string str)
  (call-with-input-string str
    (lambda (port)
      (let loop ((forms '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms))))))))

(define (definition? form)
  (and (pair? form)
       (memq (car form) '(define define-public define-syntax
                          define-syntax-rule define-module use-modules
                          define-record-type define-values))))

(define (eval-form form)
  "Compiles and runs FORM in the current module, returning its values.
Definitions are compiled as they are, so that they are made at the top
level; anything else is compiled as a thunk, so that all of its values
are returned."
  (if (definition? form)
      (compile form #:env (current-module) #:opts %debug-compile-options)
      ((compile `(lambda () ,form) #:env (current-module)
                #:opts %debug-compile-options))))

(define (eval-forms str)
  "Compiles and runs the expressions in STR, one at a time, in the
current module, as Guile's REPL does.  Returns the values of the last."
  (let loop ((forms (read-all-from-string str)))
    (cond
     ((null? forms) *unspecified*)
     ((null? (cdr forms)) (eval-form (car forms)))
     (else
      (eval-form (car forms))
      (loop (cdr forms))))))

(define (frame-locals frame top-frame?)
  "Returns an alist of the named local variables of FRAME and their
values.  If a name is bound more than once, the first binding is used."
  (let loop ((bindings (or (false-if-exception
                            (frame-bindings frame top-frame?))
                           '()))
             (locals '()))
    (if (null? bindings)
        (reverse locals)
        (let* ((binding (car bindings))
               (name (binding-name binding)))
          (if (or (not name) (assq name locals))
              (loop (cdr bindings) locals)
              (let ((value (false-if-exception (binding-ref binding))))
                (loop (cdr bindings) (acons name value locals))))))))

(define (eval-in-frame str frame top-frame?)
  "Evaluates the expressions in STR with the local variables of FRAME
in scope.  The variables are copies: assigning to them does not change
the program.  Definitions are evaluated at the top level instead."
  (let ((forms (read-all-from-string str))
        (locals (if frame (frame-locals frame top-frame?) '())))
    (cond
     ((null? forms) *unspecified*)
     ((or (null? locals) (any definition? forms))
      (eval-forms str))
     (else
      (apply (compile `(lambda ,(map car locals) ,@forms)
                      #:env (current-module))
             (map cdr locals))))))

(define (eval-and-print str)
  "Evaluates the expressions in STR and prints the results.  While the
program is paused, the local variables of the selected frame are in
scope.  Errors are reported, but do not change the mode."
  (let* ((frames (*pause-frames*))
         (frame (and (< *selected-frame* (length frames))
                     (list-ref frames *selected-frame*)))
         (top? (and (*pause-top-frame?*) (zero? *selected-frame*))))
    ;; The expression may not finish.  Stop then aborts to this prompt,
    ;; from gdn-async-poll, leaving the program paused as it was.
    (call-with-prompt %eval-tag
      (lambda ()
        (parameterize ((*in-eval* #t))
          (call-with-values
              (lambda ()
                (print-error-on-exception
                 (lambda () (eval-in-frame str frame top?))))
            print-return-values)))
      (lambda (k)
        (format #t "~&The expression was stopped.~%")))))

(define (refresh-modules!)
  "Sends the module list to the GUI, if it has changed."
  (let ((modules (gdn-module-list)))
    (unless (equal? modules *last-module-list*)
      (set! *last-module-list* modules)
      (gdn-update-modules! *gdn-module-view* modules))))

(define (refresh-views!)
  "Updates the information pages of the GUI."
  (gdn-update-environments! *gdn-environment-view* (gdn-get-environment))
  (gdn-update-traps! *gdn-trap-view* (list-traps))
  (gdn-update-threads! *gdn-thread-view*)
  (refresh-modules!))

(define (enable-trap-hooks!)
  "Guile only checks traps when the VM trace level is positive.
`with-default-trap-handler' sets it from the number of traps that
exist when it is entered, so traps added later would never fire.
Keep it positive while a program runs."
  (when (zero? (vm-trace-level))
    (set-vm-trace-level! 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REQUESTS FROM THE GUI

(define (report-new-trap idx)
  (format #t "~&Trap ~a: ~a~%" idx (trap-name idx)))

(define (lookup-binding module-name-string symbol-string)
  "Returns the variable for SYMBOL-STRING in the loaded module named by
MODULE-NAME-STRING, and the symbol, as two values, or signals an
error."
  (let* ((name (call-with-input-string module-name-string read))
         (module (and (list? name)
                      (resolve-module name #f #f #:ensure #f)))
         (symbol (string->symbol symbol-string))
         (variable (and module (module-variable module symbol))))
    (unless module
      (error "No such module" module-name-string))
    (unless variable
      (error "No such procedure" symbol-string))
    (values variable symbol)))

(define (add-binding-trap! variable name)
  "Adds a trap to the trap list for calls to the procedure that
VARIABLE holds, whichever it is at the time.  NAME is its name."
  (let* ((idx #f)
         (trap (gdn-trap-at-binding
                variable
                (lambda (frame)
                  (trap-handler frame idx (trap-name idx))))))
    (set! idx (add-trap! trap (format #f "Breakpoint at ~a" name)))
    idx))

(define (add-trap-at-procedure-call! proc)
  "Adds a trap to the trap list for calls to PROC, and returns its
index.  This replaces Guile's procedure of the same name.

If PROC is the value of a binding with its own name, as a procedure
made with `define' is, the trap is for the binding.  So it goes on
working when the program is run again, which defines the procedure
again.  Otherwise it is Guile's trap, for PROC itself."
  (call-with-values (lambda () (gdn-procedure-binding proc (current-module)))
    (lambda (variable name)
      (if variable
          (add-binding-trap! variable name)
          (guile:add-trap-at-procedure-call! proc)))))

(define (add-source-trap! file line)
  "Adds a trap at LINE (1-based) of FILE to the trap list."
  (let* ((idx #f)
         (trap (gdn-trap-at-line file line
                                 (lambda (frame)
                                   (trap-handler frame idx (trap-name idx)))
                                 (if (*paused*) (*pause-frames*) '()))))
    (set! idx (add-trap! trap (format #f "Breakpoint at ~a:~a"
                                      (basename file) line)))
    (unless (gdn-loaded-code-at-line? file line)
      (format #t "~&No code is loaded at ~a:~a yet.  The breakpoint will \
take effect if some is.~%" (basename file) line))
    idx))

(define (process-request request)
  "Handles one request from the GUI.  A request is a list whose first
element says what it is."
  (print-error-on-exception
   (lambda ()
     (case (car request)
       ;; (add-proc-trap PROCEDURE)
       ((add-proc-trap)
        (report-new-trap (add-trap-at-procedure-call! (cadr request))))
       ;; (add-proc-trap-by-name MODULE-NAME SYMBOL), both strings
       ((add-proc-trap-by-name)
        (report-new-trap
         (call-with-values
             (lambda () (lookup-binding (cadr request) (caddr request)))
           add-binding-trap!)))
       ;; (add-source-trap FILE LINE)
       ((add-source-trap)
        (report-new-trap (add-source-trap! (cadr request) (caddr request))))
       ;; (enable-trap INDEX) (disable-trap INDEX) (delete-trap INDEX)
       ((enable-trap)
        (unless (trap-enabled? (cadr request))
          (enable-trap! (cadr request))))
       ((disable-trap)
        (when (trap-enabled? (cadr request))
          (disable-trap! (cadr request))))
       ((delete-trap)
        (delete-trap! (cadr request)))
       ;; (module-procedures MODULE-NAME)
       ((module-procedures)
        (gdn-set-module-procedures! *gdn-module-view* (cadr request)
                                    (gdn-module-procedures (cadr request))))
       ;; (select-frame INDEX): the frame for evaluating expressions
       ((select-frame)
        (let ((index (cadr request))
              (frames (*pause-frames*)))
          (when (and (*paused*) (< -1 index (length frames)))
            (set! *selected-frame* index)
            (format #t "~&Expressions are now evaluated in frame ~a, at ~a~%"
                    index (frame-location (list-ref frames index))))))
       (else
        (format (current-error-port) "Unknown request ~s~%" request))))))

(define (process-requests!)
  "Handles any requests queued by the GUI."
  (let ((requests (gdn-take-requests! *gdn-lisp*)))
    (unless (null? requests)
      (for-each process-request requests)
      ;; While paused, the VM restores the trace level when the
      ;; program resumes, so leave it alone.
      (when (and (*in-run*) (not (*paused*)))
        (enable-trap-hooks!))
      (gdn-update-traps! *gdn-trap-view* (list-traps)))))

(define (get-user-input)
  "Waits for the operator's next action, handling any requests that
arrive in the meantime.  Returns a list (ACTION DATA)."
  (let loop ()
    (process-requests!)
    (let ((input (gdn-get-user-input *gdn-lisp*)))
      (process-requests!)
      (cond
       ((eq? (car input) 'wake)
        ;; The GUI also wakes the REPL when the operator presses Stop
        ;; while the program is paused.  The async it sent with that
        ;; may not have run yet, so look now.
        (gdn-async-poll)
        (loop))
       (else input)))))

(define (gdn-async-poll)
  "The GUI runs this asynchronously in the REPL thread, while a program
is running or paused.  It handles queued requests, and stops or pauses
the program if the operator asked for that.  If the operator's own
expression is being evaluated, while the program is paused, Stop stops
that instead."
  ;; Asyncs are blocked while handling requests, so that further polls
  ;; wait their turn instead of running inside this one.
  (call-with-blocked-asyncs process-requests!)
  (case (gdn-take-interrupt-request! *gdn-lisp*)
    ((stop)
     (cond
      ((*in-eval*) (abort-to-prompt %eval-tag))
      ((*in-run*) (abort-to-prompt %stop-tag))))
    ((pause)
     (when (and (*in-run*) (not (*paused*)))
       (pause-here)))
    (else #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STANDARD INPUT

;; A thread that is blocked reading a file descriptor does not run
;; asyncs, which is how Guidance stops and pauses a program.  So a
;; program waiting for input could be neither stopped nor paused.
;;
;; Guile's suspendable ports are the way around that.  With them, a
;; read from a non-blocking port that has nothing to read calls the
;; `current-read-waiter'.  Guidance's waiter waits for the program's
;; standard input a little at a time, in Scheme, where asyncs do run.

;; How long to wait at a time, in microseconds.
(define %input-wait-microseconds 100000)

(define (make-input-waiter fallback)
  "Returns a read waiter that waits for the program's standard input so
that asyncs can run meanwhile, and calls FALLBACK, the waiter that it
replaces, for any other port."
  (lambda (port)
    (if (eq? port %gdn-input-port)
        (catch 'system-error
          (lambda ()
            (select (list (fileno port)) '() '() 0 %input-wait-microseconds))
          ;; Interrupted by a signal.  The read is tried again, and
          ;; comes back here if there is still nothing.
          (lambda args #f))
        (fallback port))))

(define (call-with-interruptible-input thunk)
  "Calls THUNK with the program's standard input set up as above."
  (install-suspendable-ports!)
  (fcntl %gdn-input-port F_SETFL
         (logior O_NONBLOCK (fcntl %gdn-input-port F_GETFL)))
  (set-current-input-port %gdn-input-port)
  (parameterize ((current-read-waiter
                  (make-input-waiter (current-read-waiter))))
    (thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUN MODE

(define (exception-handler-pre-unwind key . args)
  "Runs where the exception was raised, before the stack unwinds, so
that error mode can show the stack.  Error mode only returns by
aborting to the stop prompt."
  (unless (memq key '(quit stack-overflow out-of-memory
                      read-error syntax-error))
    (error-mode key args)))

(define (exception-handler-unwind key . args)
  "Handles the exceptions that do not enter error mode."
  (case key
    ((quit)
     (format #t "The program exited~a.~%"
             (if (and (pair? args) (integer? (car args)))
                 (format #f " with status ~a" (car args))
                 "")))
    ((stack-overflow)
     (format (current-error-port) "Stack overflow!~%"))
    ((out-of-memory)
     (format (current-error-port) "Out of memory!~%"))
    (else
     (print-exception-message (current-error-port) key args)))
  (values))

(define (run-under-debugger thunk)
  "Runs THUNK in Run mode, with Guidance's error and trap handlers.
Returns THUNK's values, or no values if it was stopped or raised an
exception."
  (gdn-clear-interrupt-request! *gdn-lisp*)
  (gdn-set-input-mode! *gdn-application-window* 'run)
  (call-with-prompt %stop-tag
    (lambda ()
      (parameterize ((*in-run* #t))
        (with-default-trap-handler trap-handler
          (lambda ()
            (let ((disable-break #f))
              (dynamic-wind
                (lambda ()
                  (set! disable-break
                    (trap-at-procedure-call _break_ break-handler)))
                (lambda ()
                  (catch #t
                    (lambda ()
                      (enable-trap-hooks!)
                      (call-with-prompt %stack-tag
                        thunk
                        (lambda (k . vals) (apply values vals))))
                    exception-handler-unwind
                    exception-handler-pre-unwind))
                (lambda ()
                  (cancel-pending-steps!)
                  (disable-break))))))))
    (lambda (k . args)
      (format #t "~&Stopped.~%")
      (values))))

(define (program-name)
  "A short name for the program on the command line, or #f."
  (cond
   ((option 'script) => basename)
   ((option 'expression) "the expression given with -c")
   (else #f)))

(define (run-program)
  "Runs the program given on Guidance's command line, under the
debugger, as Guile would: with its own command line, after loading the
files given with -l, then calling the function given with -e."
  (let ((script (option 'script))
        (expression (option 'expression))
        (entry (option 'entry)))
    (cond
     ((and (not script) (not expression))
      (format #t "No program was given on the command line.~%"))
     ((and script (not (file-exists? script)))
      (format #t "Cannot find the program ~s.~%" script))
     (else
      (let ((path (and script (canonicalize-path script))))
        (format #t "Running ~a~%" (or path (program-name)))
        (run-under-debugger
         (lambda ()
           (set-program-arguments
            (cons (or script "guidance") (option 'arguments)))
           (for-each (lambda (file) (load (canonicalize-path file)))
                     (option 'load-files))
           (if path
               (load path)
               (eval-forms expression))
           (when entry
             ((module-ref (current-module) entry) (command-line))))))))))

(define (apply-command-line! args)
  "Parses Guidance's command-line ARGS and applies the options that
take effect at once."
  (set! *options* (gdn-parse-command-line args))
  (for-each (lambda (message)
              (format (current-error-port) "guidance: ~a~%" message))
            (option 'errors))
  (set! %load-path (append (option 'load-path) %load-path))
  (set! %load-compiled-path
    (append (option 'compiled-path) %load-compiled-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERROR MODE

(define (error-mode key args)
  "Shows the error and its stack, then lets the operator inspect the
program until they press Stop."
  ;; Don't trap Guidance's own code, or the operator's expressions,
  ;; while the program is paused.  Error mode never resumes the
  ;; program, so the trace level need not be restored.
  (set-vm-trace-level! 0)
  (parameterize ((*paused* #t))
    (error-mode-loop key args)))

(define (error-mode-loop key args)
  (let ((frames (gdn-error-frames %stack-tag)))
    (set! *selected-frame* 0)
    (parameterize ((*pause-frames* frames)
                   (*pause-top-frame?* #f))
      (error-mode-interact key args frames))))

(define (error-mode-interact key args frames)
  ;; The mode comes first, as in pause-interact.
  (gdn-set-input-mode! *gdn-application-window* 'error)
  (print-exception-message (current-error-port) key args)
  (gdn-update-backtrace! *gdn-backtrace-view* frames)
  (format #t "~&Entering error mode.  The Backtrace page shows the stack.~%")
  (format #t "When ready, press the Stop button to return to the REPL.~%")
  (let loop ()
    (refresh-views!)
    (display "error>" %gdn-prompt-port)
    (let* ((input (get-user-input))
           (action (car input))
           (data (cadr input)))
      (case action
        ((stop)
         (abort-to-prompt %stop-tag))
        ((eval)
         (eval-and-print data)
         (loop))
        (else
         (loop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAP MODE

;; Guile calls trap handlers from VM hooks.  While a hook runs, the VM
;; holds the trace level at zero, so Guidance's own code is not
;; trapped, and restores it when the program resumes.

(define (frame-line frame)
  (let ((source (frame-source frame)))
    (and source (source:line-for-user source))))

(define (frame-location frame)
  "A description of FRAME's source location, like \"foo.scm:12\"."
  (let ((source (frame-source frame)))
    (if (and source (source:file source))
        (format #f "~a:~a" (basename (source:file source))
                (source:line-for-user source))
        (format #f "~a" (or (frame-procedure-name frame) "an anonymous procedure")))))

(define (add-ephemeral-trap! make-trap handler)
  "Creates a trap that fires once.  MAKE-TRAP takes a handler and
returns the trap's disable thunk, as the low-level trap constructors
do.  HANDLER receives the same arguments as the trap's handler.

The trap lasts no longer than the run that it is made in: see
cancel-pending-steps!."
  (let ((disable #f))
    (set! disable
      (make-trap (lambda args
                   (set! *pending-steps* (delq disable *pending-steps*))
                   (disable)
                   (apply handler args))))
    (set! *pending-steps* (cons disable *pending-steps*))))

(define (cancel-pending-steps!)
  "Disables the one-shot traps that have not fired.  A run can end
while a step is still pending: with an error, with Stop, or because
the program finished first.  Left enabled, the step would fire in the
next run or expression, outside the program."
  (let ((pending *pending-steps*))
    (set! *pending-steps* '())
    (for-each (lambda (disable)
                ;; One trap that cannot be disabled should not keep
                ;; the others enabled, or stop the run from ending.
                (false-if-exception (disable)))
              pending)))

(define (step-frame? frame internal?)
  "True if stepping may pause in FRAME: it has source code, and is not
in Guile's own libraries, unless INTERNAL?, which is true when the
step began in them.  Stepping never pauses in Guidance's own code,
which is what runs the program."
  (and (frame-source frame)
       (not (guidance-frame? frame))
       (or internal? (not (guile-internal-frame? frame)))))

(define (add-line-step-trap! frame into?)
  "Pauses the program at the next line reached in FRAME, or in a frame
that FRAME returns to.  With INTO?, also pauses in procedures that FRAME
calls.  Code in Guile's own libraries is skipped, unless FRAME is in
it.  If the program ends first, it just ends."
  (let ((fp (frame-address frame))
        (line (frame-line frame))
        (internal? (guile-internal-frame? frame)))
    (add-ephemeral-trap!
     (lambda (handler)
       (trap-matching-instructions
        (lambda (f)
          (and (or into? (<= (frame-address f) fp))
               (step-frame? f internal?)
               (or (not (= (frame-address f) fp))
                   (not (eqv? (frame-line f) line)))))
        handler))
     step-handler)))

(define (add-next-line-trap! internal?)
  "Pauses the program at the next line it reaches, in any frame in
which stepping may pause; see step-frame?."
  (add-ephemeral-trap!
   (lambda (handler)
     (trap-matching-instructions
      (lambda (f) (step-frame? f internal?))
      handler))
   step-handler))

(define (add-instruction-step-trap! frame into? outermost)
  "Pauses the program at the next instruction in FRAME, or in a frame
it returns to.  With INTO?, also in procedures that FRAME calls.

Unlike line steps, these go through Guile's libraries too.  So that
they stay in the program, they do not go further out than OUTERMOST,
the outermost frame of the program: when that returns to the code that
runs the program, the program just runs on."
  (let ((fp (frame-address frame))
        (limit (frame-address outermost)))
    (add-ephemeral-trap!
     (lambda (handler)
       (trap-matching-instructions
        (lambda (f)
          (and (or into? (<= (frame-address f) fp))
               (>= (frame-address f) limit)
               (not (guidance-frame? f))))
        handler))
     step-handler)))

(define (add-step-out-trap! frame)
  "Pauses the program in FRAME's caller, when FRAME returns.

The caller may not be the program's: it is in Guile's libraries if
FRAME was called by, say, `map', or by `load' as the last thing in its
file.  Then the return is reported, and the program pauses at the next
line of its own that it reaches, if it reaches one."
  (let ((name (or (frame-procedure-name frame) "the procedure"))
        (internal? (guile-internal-frame? frame)))
    (add-ephemeral-trap!
     (lambda (handler)
       (trap-frame-finish frame handler (lambda (f) #t)))
     (lambda (returning-frame)
       (let* ((caller (frame-previous returning-frame))
              (values (or (false-if-exception
                           (frame-return-values returning-frame))
                          '()))
              (message (format #f "Returned from ~a~a"
                               name
                               (if (null? values)
                                   ""
                                   (format #f " with ~{~s~^, ~}" values)))))
         (if (and caller (step-frame? caller internal?))
             (pause caller
                    (format #f "~a, to ~a" message (frame-location caller))
                    "step>")
             (begin
               (format #t "~&~a~%" message)
               (add-next-line-trap! internal?))))))))

(define* (pause frame message prompt #:key (top-frame? #t))
  "Pauses the program at FRAME and lets the operator inspect it, then
continue, step, or stop.  TOP-FRAME? says whether FRAME is the frame
that is executing, as it is when a trap fires."
  (let ((frames (gdn-frame-frames frame %stack-tag)))
    (set! *selected-frame* 0)
    (parameterize ((*paused* #t)
                   (*pause-frames* frames)
                   (*pause-top-frame?* top-frame?))
      (pause-interact frame frames message prompt))))

(define (outermost-frame frame frames)
  "The outermost frame of the program paused at FRAME, given FRAMES,
its stack as shown in the backtrace: innermost first, without the
frames of the code that runs the program."
  (if (null? frames) frame (last frames)))

(define (pause-interact frame frames message prompt)
  (begin
    ;; The mode comes first: the GUI accepts the operator's actions
    ;; by the mode, and they may act as soon as they see the message.
    (gdn-set-input-mode! *gdn-application-window* 'trap)
    (format #t "~&~a~%" message)
    (gdn-update-backtrace! *gdn-backtrace-view* frames (*pause-top-frame?*))
    (let loop ()
      (refresh-views!)
      (display prompt %gdn-prompt-port)
      (let* ((input (get-user-input))
             (action (car input))
             (data (cadr input)))
        (case action
          ((run continue) #t)
          ((step) (add-line-step-trap! frame #f))
          ((step-into) (add-line-step-trap! frame #t))
          ((step-instruction)
           (add-instruction-step-trap! frame #f (outermost-frame frame frames)))
          ((step-into-instruction)
           (add-instruction-step-trap! frame #t (outermost-frame frame frames)))
          ((step-out) (add-step-out-trap! frame))
          ((stop) (abort-to-prompt %stop-tag))
          ((eval)
           (eval-and-print data)
           (loop))
          (else
           (loop)))))
    (gdn-set-input-mode! *gdn-application-window* 'run)))

(define (trap-handler frame trap-idx trap-name)
  "The handler for the traps in the trap list."
  (pause frame
         (format #f "Trap ~a: ~a, at ~a" trap-idx trap-name
                 (frame-location frame))
         (format #f "trap ~a>" trap-idx)))

(define (step-handler frame)
  (pause frame (format #f "Step to ~a" (frame-location frame)) "step>"))

(define (break-handler frame)
  "Guidance traps calls to _break_ while a program runs.  FRAME is the
frame of _break_, so pause in its caller."
  (let ((caller (or (frame-previous frame) frame)))
    (pause caller (format #f "Break at ~a" (frame-location caller))
           "break>")))

(define (pause-here)
  "Pauses the program where it is, in response to the Pause button.
This runs asynchronously, so the innermost frames are Guidance's; pause
in the innermost frame of the program instead."
  (define (program-frame? frame)
    (let ((source (frame-source frame)))
      (and source (source:file source)
           (not (guile-internal-frame? frame)))))
  (define (guidance-frame? frame)
    (memq (frame-procedure-name frame) '(pause-here gdn-async-poll)))
  ;; Walk outward from here, past Guidance's own frames, to the first
  ;; frame of the program.  Programs can be deep, so look no further
  ;; than needed.
  (let* ((start (let loop ((frame (stack-ref (make-stack #t) 0)))
                  (cond
                   ((not frame) #f)
                   ((guidance-frame? frame) (frame-previous frame))
                   (else (loop (frame-previous frame))))))
         (frame (let loop ((frame start) (n 0))
                  (cond
                   ((or (not frame) (> n 100)) #f)
                   ((program-frame? frame) frame)
                   (else (loop (frame-previous frame) (1+ n)))))))
    (if (not frame)
        (format #t "~&The program could not be paused here.~%")
        ;; Unlike traps, this is not called from a VM hook, so turn
        ;; off tracing while paused, and restore it afterwards.
        (let ((saved-level (vm-trace-level)))
          (set-vm-trace-level! 0)
          (pause frame (format #f "Paused at ~a" (frame-location frame))
                 "pause>" #:top-frame? #f)
          (set-vm-trace-level! saved-level)))))

(define (caller-location)
  "The source location of the code that called the procedure calling
this one, like \"foo.scm:12\", or #f if it has no source file."
  ;; Frame 0 is make-stack, 1 this procedure, 2 its caller.
  (let* ((stack (make-stack #t))
         (frame (and (> (stack-length stack) 3) (stack-ref stack 3)))
         (source (and frame (frame-source frame))))
    (and source (source:file source)
         (not (guile-internal-frame? frame))
         (frame-location frame))))

(define (_pk_ . values)
  "Like Guile's pk, for \"peek\": logs VALUES on Guidance's Peek page,
with where it was called from, and returns the last of them."
  (gdn-add-peek! *gdn-application-window*
                 (let ((location (caller-location)))
                   (if location
                       (format #f "~a: ~s" location values)
                       (format #f "~s" values))))
  (if (null? values)
      *unspecified*
      (car (last-pair values))))

(define (_break_)
  "Pauses the program in trap mode at the caller of _break_, when the
program is run by Guidance.  Otherwise, does nothing."
  *unspecified*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVAL MODE

(define (repl-welcome)
  "A friendly welcome message printed to the console."
  (format #t "Welcome to Guidance, an alternative Guile REPL.~%")
  (format #t "This is linked to Guile ~a.~%" (version))
  (when (program-name)
    (format #t "Press Run (F5) to run ~a.~%" (program-name)))
  (newline))

(define (repl-loop)
  "Reads and runs the operator's expressions and commands until a quit
command arrives.  Returns the exit status."
  (let loop ()
    (gdn-set-input-mode! *gdn-application-window* 'repl)
    (refresh-views!)
    (display "repl>" %gdn-prompt-port)
    (let* ((input (get-user-input))
           (action (car input))
           (data (cadr input)))
      (case action
        ((quit)
         (if (integer? data) data 0))
        ((run)
         (run-program)
         (loop))
        ((eval)
         (call-with-values
             (lambda ()
               (run-under-debugger
                (lambda () (eval-forms data))))
           print-return-values)
         (loop))
        (else
         (loop))))))

(define (gdn-top-repl)
  "Kick off the REPL for Guidance."
  ;; Note that there is no signal handler here, because we handle
  ;; signals in the GTK main loop.
  (print-error-on-exception
   (lambda () (setlocale LC_ALL "")))
  ;; Make the trap procedures, like add-trap-at-procedure-call!,
  ;; available to the operator.
  (module-use! (current-module) (resolve-interface '(system vm trap-state)))
  (compile-for-debugging!)
  (apply-command-line! (cdr (command-line)))
  (repl-welcome)
  (let ((status (call-with-interruptible-input repl-loop)))
    (format #t "The REPL has ended with a return status of ~a.~%" status)
    status))
