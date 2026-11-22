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
  #:use-module ((rnrs io ports) #:select (make-custom-textual-output-port))
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm traps)
  #:use-module ((ice-9 threads) #:select (current-thread make-mutex thread-exited?
                                               with-mutex))
  #:use-module ((system foreign) #:select (sizeof))
  #:use-module (system vm vm)
  #:use-module ((system vm debug)
                #:select (find-program-sources source-post-pc))
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
;; THE TERMINAL
;;
;; Guidance's own messages go to the Terminal, whatever the program has
;; done with its ports.  The program may be paused while its output is
;; going into a string or a file; it may have changed the current
;; output port for good, or closed it.  None of that is Guidance's to
;; write to, or to lose its voice by.
;;
;; So there are two sets of ports to the Terminal.  Guidance writes to
;; its own, and never hands them out.  The program gets others, which
;; are made the current ports afresh for each run, and replaced if the
;; program has closed them.

;; Guidance's ports: those that are current when the REPL starts.
(define *terminal-out* #f)
(define *terminal-err* #f)
(define *terminal-warn* #f)

;; The program's ports.
(define *program-out* #f)
(define *program-err* #f)

(define (port-usable? port)
  (and port (not (port-closed? port))))

(define (carry-column! from to)
  "Lets TO know the column that FROM has left the Terminal's cursor in,
so that `~&' in a message starts a new line exactly when one is
needed."
  (when (and (port-usable? from) (port-usable? to) (not (eq? from to)))
    (set-port-column! to (port-column from))))

(define (say message . args)
  "Writes to the Terminal, as `format' does."
  (carry-column! *program-out* *terminal-out*)
  (apply format *terminal-out* message args)
  (carry-column! *terminal-out* *program-out*))

(define (say-error message . args)
  "Writes to the Terminal, as an error."
  (apply format *terminal-err* message args))

(define (use-terminal-ports!)
  "Takes the current ports as Guidance's own ports to the Terminal."
  (set! *terminal-out* (current-output-port))
  (set! *terminal-err* (current-error-port))
  ;; Warnings go where errors do, without the compiler's notes.
  (set! *terminal-warn* (compiler-note-filter *terminal-err*))
  (set! *program-out* #f)
  (set! *program-err* #f))

(define (ensure-program-ports!)
  "Makes sure that the program's ports to the Terminal exist and are
open.  Where a second port cannot be made, as in the tests, whose
Terminal is a string port, the program shares Guidance's."
  (unless (port-usable? *program-out*)
    (set! *program-out*
      (if (file-port? *terminal-out*)
          (let ((port (duplicate-port *terminal-out* "w0")))
            (set-port-encoding! port "UTF-8")
            port)
          *terminal-out*)))
  (unless (port-usable? *program-err*)
    (set! *program-err* (or (gdn-open-error-port) *terminal-err*)))
  ;; Only where the program has an error port of Guidance's own kind.
  (when (and (not *subprocess-err*)
             (not (eq? *program-err* *terminal-err*))
             (not (file-port? *program-err*)))
    (set! *subprocess-err* (fdopen (dup 2) "w0"))
    (install-subprocess-wrappers!)))

;; Subprocesses.
;;
;; Guile's `system*' and its pipes give a subprocess the current error
;; port as its error stream if that is a file port, and otherwise
;; /dev/null.  The program's error port is not a file port: it is one
;; of Guidance's, which marks what it writes as errors, so that the
;; Terminal can show output and errors in the order they were written.
;; A subprocess's errors would therefore vanish.
;;
;; So the two primitives that start subprocesses are wrapped.  When the
;; current error port is the program's Terminal port, they see instead
;; a file port for file descriptor 2, which is the Terminal's too.  If
;; the program has sent its errors elsewhere, that is left alone.

;; A file port for the Terminal's error stream, or #f where there is
;; none, as in the tests.
(define *subprocess-err* #f)

(define (with-subprocess-error-port proc)
  "Returns a procedure that calls PROC as described above."
  (lambda args
    (if (and *subprocess-err*
             (eq? (current-error-port) *program-err*)
             (not (file-port? *program-err*)))
        (parameterize ((current-error-port *subprocess-err*))
          (apply proc args))
        (apply proc args))))

(define *subprocess-wrappers-installed?* #f)

(define (install-subprocess-wrappers!)
  "Wraps `system*', and `piped-process', which Guile's pipes are made
with.  Code refers to both through their modules' variables, so it sees
the wrappers."
  (unless *subprocess-wrappers-installed?*
    (set! *subprocess-wrappers-installed?* #t)
    (for-each
     (lambda (where)
       (let ((variable (module-variable (resolve-module (car where))
                                        (cdr where))))
         (when (and variable (variable-bound? variable))
           (variable-set! variable
                          (with-subprocess-error-port
                           (variable-ref variable))))))
     '(((guile) . system*)
       ((ice-9 popen) . piped-process)))))

(define (call-with-program-ports thunk)
  "Calls THUNK with the program's ports as the current ports.  What
THUNK does to the current ports ends with it."
  (ensure-program-ports!)
  (parameterize ((current-output-port *program-out*)
                 (current-error-port *program-err*)
                 (current-warning-port *terminal-warn*)
                 (current-input-port %gdn-input-port))
    (thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS

(define (print-error-on-exception thunk)
  "Runs THUNK.  If an error occurs, an error message is displayed on
the current error port and *unspecified* is returned."
  (catch #t
    thunk
    (lambda (key . args)
      (print-exception-message *terminal-err* key args)
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

(define (show-prompt prompt)
  "Shows PROMPT in the GUI.  Each ends with a newline, so that the GUI
can tell two prompts that arrive together apart."
  (display prompt %gdn-prompt-port)
  (newline %gdn-prompt-port))

(define (print-return-values . return-vals)
  "Prints the return values of an expression, which could be multiple
values."
  (for-each (lambda (val)
              (unless (unspecified? val)
                (say "→ ~s~%" val)))
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
          (if (or (not name) (assq name locals)
                  (not (gdn-binding-readable? frame binding)))
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
    ;; What the expression itself prints goes to the Terminal, also
    ;; when the program is paused with its output going elsewhere.
    (call-with-prompt %eval-tag
      (lambda ()
        (parameterize ((*in-eval* #t))
          (call-with-values
              (lambda ()
                (print-error-on-exception
                 (lambda ()
                   (call-with-program-ports
                    (lambda () (eval-in-frame str frame top?))))))
            print-return-values)))
      (lambda (k)
        (say "~&The expression was stopped.~%")))))

(define (refresh-modules!)
  "Sends the module list to the GUI, if it has changed."
  (let ((modules (gdn-module-list)))
    (unless (equal? modules *last-module-list*)
      (set! *last-module-list* modules)
      (gdn-update-modules! *gdn-module-view* modules))))

(define *line-breakpoints* (make-hash-table))

(define (line-breakpoints)
  "Returns the breakpoints at lines that still exist, as a list of
vectors #(INDEX FILE LINE ENABLED?)."
  (filter-map (lambda (idx)
                (let ((where (hashv-ref *line-breakpoints* idx)))
                  (and where
                       (vector idx (car where) (cdr where)
                               (trap-enabled? idx)))))
              (list-traps)))

(define (refresh-traps!)
  "Sends the trap list to the Traps page, and the breakpoints at lines
to the Source page."
  (gdn-update-traps! *gdn-trap-view* (list-traps))
  (gdn-set-source-breakpoints! *gdn-source-view* (line-breakpoints)))

(define (refresh-views!)
  "Updates the information pages of the GUI."
  (gdn-update-environments! *gdn-environment-view* (gdn-get-environment))
  (refresh-traps!)
  (gdn-update-threads! *gdn-thread-view* (thread-rows))
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
  (say "~&Trap ~a: ~a~%" idx (trap-name idx)))

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
    (hashv-set! *line-breakpoints* idx (cons file line))
    (unless (gdn-loaded-code-at-line? file line)
      (say "~&No code is loaded at ~a:~a yet.  The breakpoint will \
take effect if some is.~%" (basename file) line))
    idx))

(define (set-program! file)
  "Makes FILE the program that Run runs, in place of the one given on
the command line, and without arguments.  The other options, such as
the load path, stay as they are."
  (set! *options*
    (map (lambda (entry)
           (case (car entry)
             ((script) (cons 'script file))
             ((expression) (cons 'expression #f))
             ((arguments) (cons 'arguments '()))
             (else entry)))
         *options*))
  (say "~&Press Run (F5) to run ~a.~%" (program-name)))

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
       ;; (set-program FILE): the program that Run runs
       ((set-program)
        (set-program! (cadr request)))
       ;; (select-frame INDEX): the frame for evaluating expressions
       ((select-frame)
        (let ((index (cadr request))
              (frames (*pause-frames*)))
          (when (and (*paused*) (< -1 index (length frames)))
            (set! *selected-frame* index)
            (say "~&Expressions are now evaluated in frame ~a, at ~a~%"
                    index (frame-location (list-ref frames index))))))
       (else
        (say-error "Unknown request ~s~%" request))))))

(define (process-requests!)
  "Handles any requests queued by the GUI."
  (let ((requests (gdn-take-requests! *gdn-lisp*)))
    (unless (null? requests)
      (for-each process-request requests)
      ;; While paused, the VM restores the trace level when the
      ;; program resumes, so leave it alone.
      (when (and (*in-run*) (not (*paused*)))
        (enable-trap-hooks!))
      (refresh-traps!))))

(define (get-action)
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

(define (unfinished-expression? str)
  "True if STR ends part-way through an expression, so that more lines
are needed: reading it runs out of input inside a form or string."
  (catch 'read-error
    (lambda ()
      (call-with-input-string str
        (lambda (port)
          (let loop ()
            (unless (eof-object? (read port))
              (loop)))))
      #f)
    (lambda (key subr message args rest)
      (and (string? message)
           (string-contains message "end of input")
           #t))))

(define (get-user-input)
  "Waits for the operator's next action, handling any requests that
arrive in the meantime.  Returns a list (ACTION DATA).  An expression
entered over several lines arrives as one: while what has been entered
is unfinished, a continuation prompt asks for more.  Any other action
abandons the unfinished expression."
  (let loop ((input (get-action)))
    (if (and (eq? (car input) 'eval)
             (unfinished-expression? (cadr input)))
        (begin
          (show-prompt "...")
          (let ((next (get-action)))
            (if (eq? (car next) 'eval)
                (loop (list 'eval (string-append (cadr input) "\n"
                                                 (cadr next))))
                (begin
                  (say "~&The unfinished expression was abandoned.~%")
                  next))))
        input)))

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
                      read-error syntax-error gdn-stack-limit))
    (error-mode key args)))

(define (exception-handler-unwind key . args)
  "Handles the exceptions that do not enter error mode."
  (case key
    ((quit)
     (say "The program exited~a.~%"
             (if (and (pair? args) (integer? (car args)))
                 (format #f " with status ~a" (car args))
                 "")))
    ((stack-overflow)
     (say-error "Stack overflow!~%"))
    ;; The stack is shallow again here: see stack-limit-reached.
    ((gdn-stack-limit)
     (error-mode-after-unwind (car args)))
    ((out-of-memory)
     (say-error "Out of memory!~%"))
    (else
     (print-exception-message *terminal-err* key args)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THREADS
;;
;; Guidance debugs one thread: the one that runs the program, the REPL
;; thread.  Traps and error mode belong to it.  For other threads that
;; the program starts, Guidance at least reports what it cannot do,
;; rather than leaving Guile's own messages, or nothing.

;; While a program runs, the thread that runs it; otherwise #f.
(define *program-thread* #f)

;; call-with-new-thread from (ice-9 threads), as Guile defines it.
(define %call-with-new-thread
  (module-ref (resolve-interface '(ice-9 threads)) 'call-with-new-thread))

(define (report-thread-error key args)
  (let ((port *terminal-err*))
    (format port "~&An error in a thread of the program ended the thread:~%")
    (print-exception-message port key args)
    (format port "Guidance can pause and inspect only the program's \
main thread.~%")))

;; The threads that the program has started, newest first, as a list
;; of vectors #(THREAD NUMBER PROCEDURE-NAME), for the Threads page.
;; They are numbered from 1 in the order they were started.  Program
;; threads add to it, so it is guarded by a mutex.
(define *program-threads* '())
(define *program-thread-count* 0)
(define *program-threads-mutex* (make-mutex))

(define (procedure-display-name proc)
  "The name of PROC, as the Backtrace page shows it: λ if it has none."
  (let ((name (and (procedure? proc) (procedure-name proc))))
    (if (and name (not (eq? name '_)))
        (symbol->string name)
        "λ")))

(define (add-program-thread! thread thunk)
  (with-mutex *program-threads-mutex*
    (set! *program-thread-count* (1+ *program-thread-count*))
    (set! *program-threads*
      (cons (vector thread *program-thread-count*
                    (procedure-display-name thunk))
            *program-threads*))))

(define (forget-finished-threads!)
  "Removes the threads that have finished from the Threads page, as a
new run of the program starts.  When none is left, numbering starts
again from 1."
  (with-mutex *program-threads-mutex*
    (set! *program-threads*
      (filter (lambda (entry) (not (thread-exited? (vector-ref entry 0))))
              *program-threads*))
    (when (null? *program-threads*)
      (set! *program-thread-count* 0))))

(define (thread-rows)
  "The rows of the Threads page, as a vector of vectors #(NAME
PROCEDURE STATE): the program's main thread, which is the REPL's, then
the threads the program started.  Guidance's own threads, and Guile's,
are left out."
  (list->vector
   (cons (vector "Main thread"
                 (or (program-name) "REPL")
                 (cond ((*paused*) "paused")
                       ((*in-run*) "running")
                       (else "waiting at the REPL")))
         (map (lambda (entry)
                (vector (format #f "Thread ~a" (vector-ref entry 1))
                        (vector-ref entry 2)
                        (if (thread-exited? (vector-ref entry 0))
                            "finished"
                            "running")))
              (reverse
               (with-mutex *program-threads-mutex* *program-threads*))))))

(define* (debugger-call-with-new-thread thunk #:optional handler)
  "call-with-new-thread, as used while Guidance runs a program.  If the
thread raises an error that it does not handle, the error is reported
in a few lines, and the thread ends.  The thread is listed on the
Threads page."
  (let ((thread
         (if handler
             (%call-with-new-thread thunk handler)
             (%call-with-new-thread
              (lambda ()
                (catch #t
                  thunk
                  (lambda (key . args)
                    (unless (eq? key 'quit)
                      (report-thread-error key args))
                    #f)))))))
    (add-program-thread! thread thunk)
    thread))

(define (call-with-thread-reports thunk)
  "Calls THUNK with call-with-new-thread replaced by
debugger-call-with-new-thread, for the program, and the REPL thread
marked as the program's.  Code that refers to call-with-new-thread,
including the expansion of begin-thread, uses the variable of (ice-9
threads), so it sees the replacement."
  (let ((variable (module-variable (resolve-module '(ice-9 threads))
                                   'call-with-new-thread)))
    (dynamic-wind
      (lambda ()
        (set! *program-thread* (current-thread))
        (variable-set! variable debugger-call-with-new-thread))
      thunk
      (lambda ()
        (variable-set! variable %call-with-new-thread)
        (set! *program-thread* #f)))))

;; The most stack a program may use, in words: 256 MiB on a 64-bit
;; machine, enough for some eleven million frames of a simple
;; recursive procedure.  Past that, the program is taken to be
;; recursing without end, and enters error mode.  Without a limit of
;; its own, Guile lets the stack grow until memory runs short, which
;; under Guidance took minutes.
(define %stack-limit (* 32 1024 1024))

(define (stack-limit-reached)
  "Guile calls this where a program exceeded %stack-limit.  Error mode
cannot stay here: Stop would unwind out of this handler, at which Guile
puts the program's limit back while the stack is still deep, and the
limit would be exceeded again.  So the backtrace is shown now, while
the frames exist, and the program is unwound first; error mode then
runs from exception-handler-unwind."
  (set-vm-trace-level! 0)
  (print-error-on-exception
   (lambda ()
     (gdn-update-backtrace! *gdn-backtrace-view*
                            (gdn-error-frames %stack-tag))))
  (throw 'gdn-stack-limit
         (format #f "Stack overflow: the program used more than ~a MiB \
of stack, so it is probably recursing without end."
                 (quotient (* %stack-limit (sizeof '*)) (* 1024 1024)))))

(define (run-under-debugger thunk)
  "Runs THUNK in Run mode, with Guidance's error and trap handlers.
Returns THUNK's values, or no values if it was stopped or raised an
exception."
  (gdn-clear-interrupt-request! *gdn-lisp*)
  (hash-clear! *entry-kinds*)
  (gdn-set-input-mode! *gdn-application-window* 'run)
  (call-with-prompt %stop-tag
    (lambda ()
      (parameterize ((*in-run* #t))
        (with-default-trap-handler trap-handler
          (lambda ()
            (dynamic-wind
                (lambda () #t)
                (lambda ()
                  (catch #t
                    (lambda ()
                      (enable-trap-hooks!)
                      (call-with-thread-reports
                       (lambda ()
                         (call-with-stack-overflow-handler %stack-limit
                           (lambda ()
                             (call-with-prompt %stack-tag
                               (lambda () (call-with-program-ports thunk))
                               (lambda (k . vals) (apply values vals))))
                           stack-limit-reached))))
                    exception-handler-unwind
                    exception-handler-pre-unwind))
                (lambda ()
                  (cancel-pending-steps!)))))))
    (lambda (k . args)
      (say "~&Stopped.~%")
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
      (say "No program was given on the command line.~%"))
     ((and script (not (file-exists? script)))
      (say "Cannot find the program ~s.~%" script))
     (else
      (let ((path (and script (canonicalize-path script))))
        (forget-finished-threads!)
        (say "Running ~a~%" (or path (program-name)))
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
             ((if (symbol? entry)
                  (module-ref (current-module) entry)
                  (eval entry (current-module)))
              (command-line))))))))))

(define (apply-command-line! args)
  "Parses Guidance's command-line ARGS and applies the options that
take effect at once."
  (set! *options* (gdn-parse-command-line args))
  (for-each (lambda (message)
              (say "guidance: ~a~%" message))
            (option 'notes))
  (for-each (lambda (message)
              (say-error "guidance: ~a~%" message))
            (option 'errors))
  (set! %load-path (append (option 'load-path) %load-path))
  (set! %load-compiled-path
    (append (option 'compiled-path) %load-compiled-path))
  (set! %load-extensions (append (option 'extensions) %load-extensions))
  (when (option 'fresh-auto-compile?)
    (set! %fresh-auto-compile #t))
  ;; As Guile's --use-srfi does, for the program and the operator.
  (for-each (lambda (n)
              (module-use! (resolve-module '(guile-user))
                           (resolve-interface
                            (list 'srfi
                                  (string->symbol
                                   (format #f "srfi-~a" n))))))
            (option 'srfis)))

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
  (print-exception-message *terminal-err* key args)
  (gdn-update-backtrace! *gdn-backtrace-view* frames)
  (say "~&Entering error mode.  The Backtrace page shows the stack.~%")
  (say "When ready, press the Stop button to return to the REPL.~%")
  (let loop ()
    (refresh-views!)
    (show-prompt "error>")
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

(define (error-mode-after-unwind message)
  "Error mode for an error whose stack is gone, though the Backtrace
page still shows it.  Expressions are evaluated at the top level."
  (set-vm-trace-level! 0)
  (set! *selected-frame* 0)
  (parameterize ((*paused* #t)
                 (*pause-frames* '())
                 (*pause-top-frame?* #f))
    (gdn-set-input-mode! *gdn-application-window* 'error)
    (say-error "~&~a~%" message)
    (say "~&Entering error mode.  The Backtrace page shows where \
the program was; it has been unwound, so expressions are evaluated at \
the top level.~%")
    (say "When ready, press the Stop button to return to the REPL.~%")
    (let loop ()
      (print-error-on-exception refresh-views!)
      (show-prompt "error>")
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
           (loop)))))))

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
which is what runs the program.

This looks up FRAME's debugging information, so it must not be called
from an instruction hook: see add-step-trap!."
  (and (frame-source frame)
       (not (guidance-frame? frame))
       (or internal? (not (guile-internal-frame? frame)))))

;; Low-level traps on the VM's hooks.  Each has the interface of Guile's
;; low-level traps: calling it disables it, and returns a procedure that
;; enables it again.

(define (hook-trap add-hook! remove-hook! hook)
  (add-hook! hook)
  (lambda* (#:optional frame)
    (remove-hook! hook)
    (lambda* (#:optional frame)
      (hook-trap add-hook! remove-hook! hook))))

(define (combine-traps traps)
  (lambda* (#:optional frame)
    (for-each (lambda (trap) (trap frame)) traps)
    (lambda* (#:optional frame) #f)))

;; Stepping.
;;
;; Guile runs an instruction hook before every instruction of the
;; thread, in whatever state the VM is in between them.  Looking up
;; debugging information there, as frame-source and
;; frame-procedure-name do, corrupts the program or crashes Guile when
;; it happens while an exception is being raised or caught: values
;; become #<unspecified>.  The apply, return and abort hooks run
;; between procedures, where it is safe.
;;
;; So a step's instruction hook only compares addresses: it watches
;; the one frame being stepped, whose code and current line are looked
;; up beforehand, while the program is paused.  The other hooks decide
;; everything else: whether a procedure being entered, or a frame
;; being returned or aborted to, belongs to the program.

(define (frame-code-extent frame)
  "The start and end addresses of the code of FRAME's procedure, or #f
and #f."
  (code-extent (frame-instruction-pointer frame)))

(define (current-position-range frame start end)
  "The instruction range, as a list of one (START . END) pair, of the
source position, line and column, that FRAME is at, in its procedure,
whose code runs from START to END."
  (let loop ((sources (if start (find-program-sources start) '()))
             (found #f))
    (cond
     ((null? sources)
      (if found (list (cons found end)) '()))
     ((<= (source-post-pc (car sources)) (frame-instruction-pointer frame))
      (loop (cdr sources) (source-post-pc (car sources))))
     (found
      (list (cons found (source-post-pc (car sources)))))
     (else '()))))

(define (current-line-ranges frame start)
  "The instruction ranges of the line FRAME is at, in its procedure,
whose code starts at START."
  (let ((source (frame-source frame)))
    (if (and start source (source:file source))
        (code-line-ranges start (source:file source) (source:line source))
        '())))

(define (position-starts start)
  "A table of the addresses at which a source position begins, in the
procedure whose code starts at START."
  (let ((table (make-hash-table)))
    (for-each (lambda (source)
                (hashv-set! table (source-post-pc source) #t))
              (if start (find-program-sources start) '()))
    table))

;; How many of the frames outside the stepped one a step prepares to
;; land in; see add-step-trap!.
(define %step-landing-frames 30)

;; What kind of code a procedure is, by the address it starts at:
;; `program', `library' (Guile's own), `guidance', or `none' if it has
;; no source.  Step Into asks this of every procedure that is entered,
;; and looking it up each time made a step through library code that
;; calls a great deal take minutes.  Emptied when a run starts.
(define *entry-kinds* (make-hash-table))

(define (entry-kind frame)
  "The kind of the procedure that FRAME, which is being entered, runs."
  (let ((ip (frame-instruction-pointer frame)))
    (or (hashv-ref *entry-kinds* ip)
        (let ((kind (cond
                     ((not (frame-source frame)) 'none)
                     ((guidance-frame? frame) 'guidance)
                     ((guile-internal-frame? frame) 'library)
                     (else 'program))))
          (hashv-set! *entry-kinds* ip kind)
          kind))))

(define (add-step-trap! frame into? expression?)
  "Pauses the program at the next line reached in FRAME, or, with
EXPRESSION?, at the next source position, line and column.

Steps pause where a source position begins, never at an instruction
in the middle of one.  In the middle of an expression, the VM can be
part way through building an object: between the instruction that
allocates a pair and those that fill it in, say.  Showing such an
object, as the Backtrace page does, can crash Guile.  And it can be
part way through delivering values: when a continuation is called,
the frame it returns into resumes in the middle of an expression,
with the continuation's values not yet received, and pausing there
loses them.  Where a source position begins, its predecessors are
complete.

If FRAME returns, pauses in its caller, if that is the program's.
With INTO?, also pauses on entering a procedure of the program that
FRAME calls, directly or through Guile's libraries.  If a continuation
or an exception takes the program to a frame outside FRAME, the step
goes on in that frame.  Once the step leaves the program, it pauses at
the first code of the program that runs again, if any; if the program
ends first, it just ends.  Code in Guile's own libraries is skipped,
unless FRAME is in it.

Hooks are costly, and the more so the deeper the stack: each call of
one takes time in proportion to the stack's depth.  So a step keeps
installed only the hooks that it needs at the time.  The instruction
hook, which runs most often by far, is needed only while FRAME itself
is running; while a procedure that FRAME called runs, it is removed,
and so is the apply hook unless INTO?."
  (let* ((fp (frame-address frame))
         (internal? (guile-internal-frame? frame))
         (in-frame? #t)
         (start #f)
         (end #f)
         (starts #f)
         ;; The instruction ranges of where the step began.
         (here '())
         ;; While a procedure that FRAME called is running, the address
         ;; of its frame; otherwise #f.
         (callee #f)
         ;; The frames outside FRAME that the step can land in, by
         ;; address: each a vector #(START END STARTS).  They are looked
         ;; up now, while the program is paused, because the abort hook
         ;; must do no more than compare addresses.
         (landings (make-hash-table))
         (handler #f)
         (next-on? #f)
         (apply-on? #f))
    (define (program? f)
      (and f (step-frame? f internal?)))
    (define (entering-program? f)
      (case (entry-kind f)
        ((program) #t)
        ((library) internal?)
        (else #f)))
    (define (look-up f)
      (call-with-values (lambda () (frame-code-extent f))
        (lambda (start end)
          (vector start end (position-starts start)))))
    (define (aim! address info)
      (set! fp address)
      (set! start (vector-ref info 0))
      (set! end (vector-ref info 1))
      (set! starts (vector-ref info 2)))

    ;; In the stepped frame: addresses only.
    (define (next-hook f)
      (when (and in-frame?
                 start
                 (= (frame-address f) fp)
                 (let ((ip (frame-instruction-pointer f)))
                   (and (<= start ip) (< ip end)
                        (not (ip-in-ranges? ip here))
                        (hashv-ref starts ip))))
        (handler f)))
    ;; Entering a procedure: one that FRAME calls; one that replaces
    ;; FRAME, by a tail call; or, once the step has left the program,
    ;; any.
    (define (apply-hook f)
      (let ((address (frame-address f)))
        (cond
         ((not in-frame?)
          (when (entering-program? f) (handler f)))
         ((= address fp)
          (when (entering-program? f) (handler f)))
         ((> address fp)
          (cond
           ((and into? (entering-program? f))
            (handler f))
           ((not callee)
            ;; FRAME has called something that the step passes over.
            (set! callee address)
            (watch-instructions! #f)
            (unless into? (watch-applies! #f))))))))
    ;; Returning: to FRAME, from what it called; from FRAME, to its
    ;; caller; or, once the step has left the program, from anything
    ;; to the program.
    (define (return-hook f)
      (let ((address (frame-address f)))
        (cond
         ((not in-frame?)
          (let ((caller (frame-previous f)))
            (when (program? caller) (handler caller #f))))
         ((and callee (= address callee))
          (back-in-frame!))
         ((= address fp)
          (let ((caller (frame-previous f)))
            (if (program? caller)
                (handler caller #f)
                (leave-program!)))))))
    ;; A continuation called, or an exception caught, that takes the
    ;; program out of what FRAME called, or to a frame outside FRAME.
    ;; The values on their way there have not arrived, so this must not
    ;; pause, nor look anything up.  The step goes on in the frame that
    ;; was landed in, to the next source position that begins there;
    ;; or, if that frame is not one that was prepared, until the
    ;; program is next entered or returned to.
    (define (abort-hook f)
      (let ((address (frame-address f)))
        (cond
         ((and in-frame? (= address fp))
          (back-in-frame!))
         ((or (not in-frame?) (< address fp))
          (let ((info (hashv-ref landings address)))
            (cond
             (info
              (aim! address info)
              (set! here '())
              (set! in-frame? #t)
              (back-in-frame!))
             (else
              (leave-program!))))))))

    (define (watch-instructions! on?)
      (unless (eq? on? next-on?)
        (set! next-on? on?)
        (if on?
            (vm-add-next-hook! next-hook)
            (vm-remove-next-hook! next-hook))))
    (define (watch-applies! on?)
      (unless (eq? on? apply-on?)
        (set! apply-on? on?)
        (if on?
            (vm-add-apply-hook! apply-hook)
            (vm-remove-apply-hook! apply-hook))))
    (define (back-in-frame!)
      (set! callee #f)
      (watch-instructions! #t)
      (watch-applies! #t))
    (define (leave-program!)
      (set! in-frame? #f)
      (set! callee #f)
      (watch-instructions! #f)
      (watch-applies! #t))

    (aim! fp (look-up frame))
    (set! here (if expression?
                   (current-position-range frame start end)
                   (current-line-ranges frame start)))
    (let loop ((frames (*pause-frames*)) (n 0))
      (when (and (pair? frames) (< n %step-landing-frames))
        (let ((f (car frames)))
          (cond
           ((>= (frame-address f) fp)
            (loop (cdr frames) n))
           ((program? f)
            (hashv-set! landings (frame-address f) (look-up f))
            (loop (cdr frames) (1+ n)))
           (else
            (loop (cdr frames) n))))))
    (add-ephemeral-trap!
     (lambda (fire)
       (set! handler fire)
       (watch-instructions! #t)
       (watch-applies! #t)
       (vm-add-return-hook! return-hook)
       (vm-add-abort-hook! abort-hook)
       ;; The trap interface: calling it disables the trap.
       (lambda* (#:optional frame)
         (watch-instructions! #f)
         (watch-applies! #f)
         (vm-remove-return-hook! return-hook)
         (vm-remove-abort-hook! abort-hook)
         (lambda* (#:optional frame) #f)))
     step-handler)))

(define (add-line-step-trap! frame into?)
  "Pauses the program at the next line; see add-step-trap!."
  (add-step-trap! frame into? #f))

(define (add-expression-step-trap! frame into?)
  "Pauses the program at the next source position; see add-step-trap!."
  (add-step-trap! frame into? #t))

(define (add-next-line-trap! internal?)
  "Pauses the program at the next code of its own that runs: a
procedure of the program being entered, or a frame of the program
being returned or aborted to.  Uses no instruction hook; see
add-step-trap!."
  (define (program? f) (and f (step-frame? f internal?)))
  (add-ephemeral-trap!
   (lambda (handler)
     (combine-traps
      (list
       (hook-trap vm-add-apply-hook! vm-remove-apply-hook!
                  (lambda (f)
                    (when (case (entry-kind f)
                            ((program) #t)
                            ((library) internal?)
                            (else #f))
                      (handler f))))
       (hook-trap vm-add-return-hook! vm-remove-return-hook!
                  (lambda (f)
                    (let ((caller (frame-previous f)))
                      (when (program? caller) (handler caller #f)))))
       (hook-trap vm-add-abort-hook! vm-remove-abort-hook!
                  (lambda (f) (when (program? f) (handler f #f)))))))
   step-handler))

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
             ;; The returning frame is still on the stack.
             (pause caller
                    (format #f "~a, to ~a" message (frame-location caller))
                    "step>" #:top-frame? #f)
             (begin
               (say "~&~a~%" message)
               (add-next-line-trap! internal?))))))))

(define* (pause frame message prompt #:key (top-frame? #t))
  "Pauses the program at FRAME and lets the operator inspect it, then
continue, step, or stop.  TOP-FRAME? says whether FRAME is the frame
that is executing, as it is when an instruction or apply hook fires.
It must be false for a caller whose callee's frame is still on the
stack: Guile then lists the values the pending call will define, whose
slots hold garbage, and printing one can crash Guile."
  (let ((frames (or (false-if-exception (gdn-frame-frames frame %stack-tag))
                    (list frame))))
    (set! *selected-frame* 0)
    (parameterize ((*paused* #t)
                   (*pause-frames* frames)
                   (*pause-top-frame?* top-frame?))
      (pause-interact frame frames message prompt))))

(define (pause-interact frame frames message prompt)
  (begin
    ;; The mode comes first: the GUI accepts the operator's actions
    ;; by the mode, and they may act as soon as they see the message.
    (gdn-set-input-mode! *gdn-application-window* 'trap)
    (say "~&~a~%" message)
    ;; This runs in a VM hook, through which an exception must not
    ;; unwind, so a failure to show something is reported instead.
    (print-error-on-exception
     (lambda ()
       (gdn-update-backtrace! *gdn-backtrace-view* frames
                              (*pause-top-frame?*))))
    (let loop ()
      (print-error-on-exception refresh-views!)
      (show-prompt prompt)
      (let* ((input (get-user-input))
             (action (car input))
             (data (cadr input)))
        (case action
          ((run continue) #t)
          ((step) (add-line-step-trap! frame #f))
          ((step-into) (add-line-step-trap! frame #t))
          ;; The GUI's "instruction" steps go by source position; see
          ;; add-step-trap!.
          ((step-instruction)
           (add-expression-step-trap! frame #f))
          ((step-into-instruction)
           (add-expression-step-trap! frame #t))
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

(define* (step-handler frame #:optional (top-frame? #t))
  "Pauses at the end of a step.  TOP-FRAME? is false when FRAME is not
the frame that is executing, but a caller that a frame is returning to;
see pause."
  (pause frame (format #f "Step to ~a" (frame-location frame)) "step>"
         #:top-frame? top-frame?))

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
        (say "~&The program could not be paused here.~%")
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

(define (break-in-caller)
  "Pauses the program in the caller of _break_.  This runs in the
program, not in a VM hook, so tracing is turned off while paused, and
restored afterwards, as for Pause.  _break_'s own frame is still on the
stack, so the caller is not the frame that is executing."
  (let* ((break-frame (let loop ((frame (stack-ref (make-stack #t) 0)))
                        (cond
                         ((not frame) #f)
                         ((eq? (frame-procedure-name frame) '_break_) frame)
                         (else (loop (frame-previous frame))))))
         (caller (and break-frame (frame-previous break-frame)))
         (saved-level (vm-trace-level)))
    (when caller
      (set-vm-trace-level! 0)
      (pause caller (format #f "Break at ~a" (frame-location caller))
             "break>" #:top-frame? #f)
      (set-vm-trace-level! saved-level))))

(define (_break_)
  "Pauses the program in trap mode at the caller of _break_, when the
program is run by Guidance.  Otherwise, does nothing.

It pauses by itself, rather than through a trap on its calls: such a
trap uses the VM's apply hook, which Guile then calls on every call the
program makes, and which made programs many times slower."
  (cond
   ((not *program-thread*) #t)
   ((not (eq? (current-thread) *program-thread*))
    (say-error "~&_break_ was called in a thread other \
than the program's main thread, and ignored: Guidance can pause only \
that one.~%"))
   ;; In an expression entered while the program is paused.
   ((*paused*) #t)
   (else (break-in-caller)))
  *unspecified*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVAL MODE

(define (repl-welcome)
  "A friendly welcome message printed to the console."
  (say "Welcome to Guidance, an alternative Guile REPL.~%")
  (say "This is linked to Guile ~a.~%" (version))
  (when (program-name)
    (say "Press Run (F5) to run ~a.~%" (program-name)))
  (say "~%"))

(define (repl-loop)
  "Reads and runs the operator's expressions and commands until a quit
command arrives.  Returns the exit status."
  (let loop ()
    (gdn-set-input-mode! *gdn-application-window* 'repl)
    (refresh-views!)
    (show-prompt "repl>")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILER NOTES

;; The lines that Guile prints, by the warning port, when it compiles
;; a file automatically.  They say nothing about the program, and
;; their cache paths are long.  The compiler's warnings are kept.
(define %compiler-note-prefixes
  '(";;; compiling "
    ";;; compiled "
    ";;; note: auto-compilation is enabled"
    ";;;       or pass the --no-auto-compile argument"
    ";;; note: source file "
    ";;;       newer than compiled "))

(define (compiler-note? line)
  (any (lambda (prefix) (string-prefix? prefix line))
       %compiler-note-prefixes))

(define (compiler-note-filter port)
  "Returns a port that passes what is written to it on to PORT, except
the compiler's notes.  A line is held back only while it may still turn
out to be a note: until its newline, if it begins with \";;; \"."
  (define pending "")
  (define (maybe-note? text)
    (if (< (string-length text) 4)
        (string-prefix? text ";;; ")
        (string-prefix? ";;; " text)))
  (define (emit! text)
    (unless (string-null? text)
      (display text port)))
  (make-custom-textual-output-port
   "warnings"
   (lambda (str start count)
     (let loop ((text (string-append pending
                                     (substring str start (+ start count)))))
       (set! pending "")
       (let ((newline (string-index text #\newline)))
         (cond
          (newline
           (let ((line (substring text 0 (1+ newline))))
             (unless (compiler-note? line)
               (emit! line))
             (loop (substring text (1+ newline)))))
          ((maybe-note? text)
           (set! pending text))
          (else
           (emit! text)))))
     count)
   #f #f #f))

(define (gdn-top-repl)
  "Kick off the REPL for Guidance."
  ;; Note that there is no signal handler here, because we handle
  ;; signals in the GTK main loop.
  (print-error-on-exception
   (lambda () (setlocale LC_ALL "")))
  ;; Make the trap procedures, like add-trap-at-procedure-call!,
  ;; available to the operator.
  (module-use! (current-module) (resolve-interface '(system vm trap-state)))
  (use-terminal-ports!)
  (current-warning-port *terminal-warn*)
  (compile-for-debugging!)
  (apply-command-line! (cdr (command-line)))
  (repl-welcome)
  (let ((status (call-with-interruptible-input repl-loop)))
    (say "The REPL has ended with a return status of ~a.~%" status)
    status))
