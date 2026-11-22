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
  #:use-module (ice-9 eval-string)
  #:use-module (ice-9 format)
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

(define (eval-and-print str)
  "Evaluates the expressions in STR and prints the results. Errors are
reported, but do not change the mode."
  (call-with-values
      (lambda ()
        (print-error-on-exception
         (lambda () (eval-string str #:compile? #t))))
    print-return-values))

(define (refresh-views!)
  "Updates the information pages of the GUI."
  (gdn-update-environments! *gdn-environment-view* (gdn-get-environment))
  (gdn-update-traps! *gdn-trap-view* (list-traps))
  (gdn-update-threads! *gdn-thread-view*))

(define (enable-trap-hooks!)
  "Guile only checks traps when the VM trace level is positive.
`with-default-trap-handler' sets it from the number of traps that
exist when it is entered, so traps added later would never fire.
Keep it positive while a program runs."
  (when (zero? (vm-trace-level))
    (set-vm-trace-level! 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REQUESTS FROM THE GUI

(define (process-request request)
  (print-error-on-exception
   (lambda ()
     (case (car request)
       ((add-proc-trap)
        (let ((idx (add-trap-at-procedure-call! (cadr request))))
          (format #t "Trap ~a: ~a~%" idx (trap-name idx))))
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
is running.  It handles queued requests, and stops the program if the
operator asked for that."
  (process-requests!)
  (when (and (gdn-take-stop-request! *gdn-lisp*)
             (*in-run*))
    (abort-to-prompt %stop-tag)))

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
  (gdn-clear-stop-request! *gdn-lisp*)
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
  (print-exception-message (current-error-port) key args)
  (gdn-update-backtrace! *gdn-backtrace-view* (gdn-error-frames %stack-tag))
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

(define (pause frame message prompt)
  "Pauses the program at FRAME and lets the operator inspect it, then
continue, step, or stop."
  (parameterize ((*paused* #t))
    (format #t "~&~a~%" message)
    (gdn-update-backtrace! *gdn-backtrace-view*
                           (gdn-frame-frames frame %stack-tag)
                           #t)
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
                (lambda () (eval-string data #:compile? #t))))
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
