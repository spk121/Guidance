;;; (guidance repl)

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
;;; GUI.

;;; Goals
;;; - Use only functions documented in the reference manual
;;; - Be simple

(use-modules (ice-9 regex)
             (ice-9 threads)
             (ice-9 session))

(export (top-repl
         _break_))

(define (print-error-on-exception thunk)
  "Runs THUNK. If an error occurs, an error is displayed on the
current error port."
  (catch #t
    thunk
    (lambda (key subr message args rest)
      (display-error #f
                     (current-error-port)
                     subr
                     message
                     args
                     rest)
      *unspecified*)))

(define (top-repl)
  "Kick off the REPL for Guidance."

  ;; Note that there is no signal handler here, because we handle
  ;; signals in the GTK main loop.
  
  ;; Install the locale.
  (print-error-on-exception
   (lambda () (setlocale LC_ALL "")))
        
  ;; Run the Guidance REPL
  (let ((status (start-repl)))

    ;; FIXME: here is where we'd put an exit hook.
    ;;   (run-hook exit-hook)
          
    (format #t
            "The REPL has ended with a return status of ~a.~%"
            status)
    status))

(define (start-repl)
  "The REPL for Guidance. We presume the locale is set and a top-level
signal handler has been set up."
  ;; Here's where Guile sets makes a REPL which is bound to a language
  ;; because it wants to use that language's built-in reader.  But
  ;; since we'll be using eval-string, we can ignore that.
  (repl-welcome)
  (run-repl))

(define (repl-welcome)
  "A friendly welcome message printed to the console."
  (format #t "Welcome to Guidance, an alternative Guile REPL.~%")
  (format #t "This is linked to Guile ~a.~%" (version))
  (newline))

(define *outer-prompt-tag* (make-prompt-tag "outer"))
(define *inner-prompt-tag* (make-prompt-tag "inner"))

(define (run-repl)
  "This is the guts of the simplified REPL. We presume locale and
signal is set up and that the language is available."
  (call-with-prompt *outer-prompt-tag*
    (while #t
      ;; We're at the REPL. At this point, we can evaluate
      ;; something at the GtkEntry, or we can run the
      ;; command-line arguments when the RUN button is pushed.
      (gdn-set-input-mode! *gdn-application-window* 'repl)
      (display "repl>" %gdn-prompt-port)
      (let ((input (gdn-get-user-input)))
        ;; Evaluate the input string in the current language
        ;; catching errors and traps.
        (call-with-error-and-trap-handler
         ;; Thunk to be executed
         (lambda ()
           (call-with-values
               (cond
                ;; The RUN button run the cmd line arguments
                ((eqv? (car input) 'run)
                 (gdn-set-input-mode! *gdn-application-window* 'run)
                 (start-stack #t (eval (compile-shell-switches argv) (current-module))))
                ;; Input at the GtkEntry is evalutated
                ((eqv (car input) 'eval)
                 (gdn-set-input-mode! *gdn-application-window* 'run)
                 (start-stack #t (eval-string input-string))))
             ;; Print the return, which could be multiple values.
             (lambda return-vals
               (for-each (lambda (val)
                           (unless (eq? val *unspecified*)
                             (display "→ ")
                             (write val)
                             (newline)))
                         return-vals)))))))
    
    ;; abort handler
    (lambda (k status)
      status)))

#|
(define (with-stack-and-prompt thunk)
  "Calls thunk. The stack is constrained to THUNK.
If '(abort-to-prompt \"prompt\")' is called, THUNK is restarted."
  (call-with-prompt *inner-prompt-tag*
    (lambda ()
      (start-stack #t (thunk)))
    (lambda (k proc)
      (with-stack-and-prompt (lambda () (proc k))))))
|#

(define (handle-exception key subr message args rest)
  "This exception handler rethrows on 'quit, and prints and error on
'stack-overflow, and ignore other errors."
  (when (memq key '(quit))
    (apply throw key subr message args rest))
  (when (memq key '(stack-overflow))
    (display-error #f
                   (current-error-port)
                   key
                   subr
                   message
                   args
                   rest)
    *unspecified*))

(define (handle-pre-unwind-exception key subr message args rest)
  "This pre-unwind handler launches Guidance's error-handler UX on all
errors except 'quit."
  (unless (memq key '(quit))
    (let* ((stack (make-stack #t 3)))
      (display-error
       (stack-ref stack 0)
       (current-error-port)
       key
       subr
       message
       args
       rest)
      
      (format #t "Entering error mode.~%")
      (format #t "When ready, press the STOP button to terminate.~%")
      (gdn-error-handler (stack-ref stack 0)))))

(define (handle-trap frame trap-idx trap-name)
  "This trap handler launches Guidance's trap-handler UX."
  (let ((stack (make-stack frame)))
    (if trap-idx
        (begin
          (format #t "Trap ~d: ~a~%" trap-idx trap-name)
          (format #t "Entering trap mode.~%"))
        ;; else
        (format #t "Entering break mode.~%"))
    (format #t "When ready, press the CONTINUE button to continue.~%")
    (gdn-trap-handler (stack-ref stack 0) trap-idx trap-name)))

(define (_break_)
  "This causes a break to occur at this location"
  (handle-trap #t #f "break"))

(define (call-with-error-and-trap-handler thunk)
  "This procedure calls THUNK. On error it calls ON-ERROR then POST-ERROR.
If a trap is encountered, it calls TRAP-HANDLER."

    (catch
      ;; key
      #t
      ;; thunk
      (lambda ()
        (with-default-trap-handler handle-trap thunk))
      
      ;; On-unwind handler
      handle-exception

      ;; Pre-unwind handler
      handle-pre-unwind-exception)))
