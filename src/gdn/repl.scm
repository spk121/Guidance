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
  #:use-module (srfi srfi-1)
  #:use-module (system base compile)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm traps)
  #:use-module (system vm vm)
  #:use-module (system vm trap-state)
  #:export (gdn-top-repl
            gdn-async-poll
            _break_)
  #:declarative? #f)

;; Aborting to this prompt stops the running program and returns to
;; the REPL.
(define %stop-tag (make-prompt-tag "gdn-stop"))

;; This prompt is installed directly around the program being run, so
;; that backtraces can be cut to show only the program's own frames.
(define %stack-tag (make-prompt-tag "gdn-stack"))

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

;; The module list most recently sent to the GUI.
(define *last-module-list* #f)

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
      (compile form #:env (current-module))
      ((compile `(lambda () ,form) #:env (current-module)))))

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
    (call-with-values
        (lambda ()
          (print-error-on-exception
           (lambda () (eval-in-frame str frame top?))))
      print-return-values)))

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

(define (lookup-procedure module-name-string symbol-string)
  "Returns the value of SYMBOL-STRING in the loaded module named by
MODULE-NAME-STRING, or signals an error."
  (let* ((name (call-with-input-string module-name-string read))
         (module (and (list? name)
                      (resolve-module name #f #f #:ensure #f))))
    (unless module
      (error "No such module" module-name-string))
    (module-ref module (string->symbol symbol-string))))

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
         (add-trap-at-procedure-call!
          (lookup-procedure (cadr request) (caddr request)))))
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
      (if (eq? (car input) 'wake)
          (loop)
          input))))

(define (gdn-async-poll)
  "The GUI runs this asynchronously in the REPL thread while a program
is running.  It handles queued requests, and stops or pauses the
program if the operator asked for that."
  ;; Asyncs are blocked while handling requests, so that further polls
  ;; wait their turn instead of running inside this one.
  (call-with-blocked-asyncs process-requests!)
  (case (gdn-take-interrupt-request! *gdn-lisp*)
    ((stop)
     (when (*in-run*)
       (abort-to-prompt %stop-tag)))
    ((pause)
     (when (and (*in-run*) (not (*paused*)))
       (pause-here)))
    (else #t)))

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
                  (disable-break))))))))
    (lambda (k . args)
      (format #t "~&Stopped.~%")
      (values))))

(define (run-program)
  "Loads the program named on the command line, under the debugger."
  (if (< (length (command-line)) 2)
      (format #t "No program was given on the command line.~%")
      (let ((file (cadr (command-line))))
        (if (not (file-exists? file))
            (format #t "Cannot find the program ~s.~%" file)
            (let ((path (canonicalize-path file)))
              (format #t "Running ~a~%" path)
              (run-under-debugger (lambda () (load path))))))))

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
  (print-exception-message (current-error-port) key args)
  (gdn-update-backtrace! *gdn-backtrace-view* frames)
  (format #t "~&Entering error mode.  The Backtrace page shows the stack.~%")
  (format #t "When ready, press the Stop button to return to the REPL.~%")
  (gdn-set-input-mode! *gdn-application-window* 'error)
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
do.  HANDLER receives the same arguments as the trap's handler."
  (let ((disable #f))
    (set! disable
      (make-trap (lambda args
                   (disable)
                   (apply handler args))))))

(define (add-line-step-trap! frame into?)
  "Pauses the program at the next line reached in FRAME, or in a frame
that FRAME returns to.  With INTO?, also pauses in procedures that FRAME
calls.  Code in Guile's own libraries is skipped, unless FRAME is in
it."
  (let ((fp (frame-address frame))
        (line (frame-line frame))
        (internal? (guile-internal-frame? frame)))
    (add-ephemeral-trap!
     (lambda (handler)
       (trap-matching-instructions
        (lambda (f)
          (and (or into? (<= (frame-address f) fp))
               (frame-source f)
               (or internal? (not (guile-internal-frame? f)))
               (or (not (= (frame-address f) fp))
                   (not (eqv? (frame-line f) line)))))
        handler))
     step-handler)))

(define (add-instruction-step-trap! frame into?)
  "Pauses the program at the next instruction in FRAME, or in a frame
it returns to.  With INTO?, also in procedures that FRAME calls."
  (let ((fp (frame-address frame)))
    (add-ephemeral-trap!
     (lambda (handler)
       (trap-matching-instructions
        (lambda (f) (or into? (<= (frame-address f) fp)))
        handler))
     step-handler)))

(define (add-step-out-trap! frame)
  "Pauses the program in FRAME's caller, when FRAME returns."
  (let ((name (or (frame-procedure-name frame) "the procedure")))
    (add-ephemeral-trap!
     (lambda (handler)
       (trap-frame-finish frame handler (lambda (f) #t)))
     (lambda (returning-frame)
       (let ((caller (or (frame-previous returning-frame) returning-frame))
             (values (or (false-if-exception
                          (frame-return-values returning-frame))
                         '())))
         (pause caller
                (format #f "Returned from ~a~a, to ~a"
                        name
                        (if (null? values)
                            ""
                            (format #f " with ~{~s~^, ~}" values))
                        (frame-location caller))
                "step>"))))))

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

(define (pause-interact frame frames message prompt)
  (begin
    (format #t "~&~a~%" message)
    (gdn-update-backtrace! *gdn-backtrace-view* frames (*pause-top-frame?*))
    (gdn-set-input-mode! *gdn-application-window* 'trap)
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
          ((step-instruction) (add-instruction-step-trap! frame #f))
          ((step-into-instruction) (add-instruction-step-trap! frame #t))
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
  (repl-welcome)
  (let ((status (repl-loop)))
    (format #t "The REPL has ended with a return status of ~a.~%" status)
    status))
