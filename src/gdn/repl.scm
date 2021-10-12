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
             (ice-9 format)
             (ice-9 session)
             (ice-9 control)
             (ice-9 command-line)
             (ice-9 optargs)
             (system vm vm)
             (system vm traps)
             (system vm trap-state))

(define (poop x)
  (if x
      (1+ x)
      x))

(define (_Break_)
  (format #t "_Break_ is called~%")
  #t)

(define *cmdline* (command-line))

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
  (add-trap-at-procedure-call! poop)
  ;; (add-trap-at-procedure-call! _Break_)
  ;; (trap-in-procedure _Break_ trap-handler trap-handler)
  (set-vm-trace-level! 0)
  (install-trap-handler! trap-handler)
  (run-repl '()))

(define (repl-welcome)
  "A friendly welcome message printed to the console."
  (format #t "Welcome to Guidance, an alternative Guile REPL.~%")
  (format #t "This is linked to Guile ~a.~%" (version))
  (newline))

(define (repl-read repl)
  (catch #t
    (lambda ()
      (gdn-set-input-mode! *gdn-application-window* 'repl)
      (display "repl>" %gdn-prompt-port)
      (gdn-update-environments! *gdn-environment-view* (gdn-get-environment))
      (gdn-update-traps! *gdn-trap-view* (list-traps))
      (gdn-update-threads)
      (pk (gdn-get-user-input *gdn-lisp*)))
    (lambda (key subr msg args rest)
      (case key
        ((quit)
         (throw key subr msg args rest))
        (else
         (format (current-error-port) "While reading expression:\n")
         (display-error #f
                        (current-error-port)
                        subr
                        msg
                        args
                        rest)
         (flush-all-input)
         *unspecified*)))))

(define (run-repl repl)
  (run-repl* repl repl-read))

(define (run-repl* repl repl-read)
  "This is the guts of the simplified REPL. We presume locale and
signal is set up and that the language is available."
  (let ((iter 0))
    (%
     (begin
       ;; Print a welcome message the first time.
       (when (zero? iter)
         (repl-welcome))
       (set! iter (1+ iter))
     
       (while #t
         (let* ((input (repl-read repl))  ; Get toolbar or GtkEntry input
                (action (car input))
                (data (cadr input)))
           (pk 'input 'action action 'data data)
           (cond
            ((eqv? action #f))               ; unknown action: ignore
            ((eqv? action 'quit)
             (abort '()))
            (else
             (execute-input-and-print-return-values action data)
             #; (call-with-minimal-error-handler
              (lambda ()
                (pk 'before-execute action data)
                (execute-input-and-print-return-values action data))))))))

     ;; Handler for the '%' prompt
     (lambda (k . status)
       status))))

(define (execute-input-and-print-return-values action data)
  (call-with-values
      (lambda ()
        (execute-input action data))
    print-return-values))
  
(define (execute-input action data)
  ;; When the Run Button is pressed, we execute the
  ;; command line. Otherwise we execute the text
  ;; from the Terminal's GtkEntry.
  
  ;; FIXME: add before-eval-hook
  (catch
    ;; key
    #t
    ;; thunk
    (lambda ()
      ;;(with-default-trap-handler
      ;;trap-handler
      (dynamic-wind
        (lambda ()
          (set-vm-trace-level! 1))
        (lambda ()
          (cond
           ((eqv? action 'run)
            ;; FIXME: insert with-stack-and-prompt here?
            (eval (compile-shell-switches *cmdline*)
                  (interaction-envirnoment)))
           ((eqv? action 'eval)
            ;; FIXME: ditto?
            (eval-string data))
           (else
            *unspecified*)))
        (lambda ()
          (set-vm-trace-level! 0))))

    (lambda (key . args)
      (cond
       ((eqv? key 'quit)
        (apply throw key args))
       ((eqv? key 'stack-overflow)
        (display "Stack overflow!" %gdn-error-port)
        (newline %gdn-error-port))
       ((eqv? key 'out-of-memory)
        (display "Out of memory!" %gdn-error-port)
        (newline %gdn-error-port))
       (else
        *unspecified*)))

    (lambda (key subr message args rest)
      (format #t "ERROR HANDLER ~S ~S ~S ~S ~S~%" key subr message args rest)
      (unless (eqv? key 'quit)
        (let ((stack (make-stack #t 4)))
          (format #t "STACK ~S~%" stack)
          (display-error
           (stack-ref stack 0)
           %gdn-error-port
           subr
           message
           args
           rest)
          (flush-all-ports)
          (format #t "Entering Error Mode.~%")
          (format #t "When ready, press the STOP 🛑 button to terminate.~%")
          (gdn-update-backtrace! *gdn-backtrace-view* (stack-ref stack 0))
          (gdn-set-input-mode! *gdn-application-window* 'error)
          (display "error>" %gdn-prompt-port)
          (while #t
            (let ((input (gdn-get-user-input *gdn-lisp*)))
              (cond
               ;; The Stop Button returns to eval mode
               ((eqv? (car input) 'stop)
                (break))
               ((eqv? (car input) 'eval)
                (display "-> ")
                (write (print-error-on-exception (eval-string (cadr input))))
                (newline))
               (else
                *unspecified*))))         
          )))))
    
(define (with-stack-and-prompt thunk)
  "No idea what this tangled mess does. Stole it from (system repl
repl)"
  (call-with-prompt
      ;; Tag
      (default-prompt-tag)
    ;; Thunk
    (lambda ()
      (start-stack #t (thunk)))
    ;; Handler
    (lambda (state-of-computation proc)
      (with-stack-and-prompt
       (lambda ()
         (proc state-of-computation))))))

(define (call-with-minimal-error-handler thunk)
  "This procedure calls THUNK. On some critical errors, it prints a brief
error message. Traps are disabled."
  (pk 'charlie)
  (catch
    ;; key
    #t
    ;; thunk
    (lambda ()
      (pk 'delta thunk)
      (with-default-trap-handler
       ;; Use a null trap handler to ignore traps.
       (lambda (frame trap-idx trap-name)
         #t)
       (lambda () (pk 'in-minimal (start-stack #t thunk)))))
    
    ;; On-unwind handler : catch
    (lambda (key . args)
      (pk 'in-minimal-unwind key args)
      (cond
       ((eqv? key 'quit)
        (apply throw key args))
       ((eqv? key 'stack-overflow)
        (display "Stack overflow!" %gdn-error-port)
        (newline %gdn-error-port))
       ((eqv? key 'out-of-memory)
        (display "Out of memory!" %gdn-error-port)
        (newline %gdn-error-port))
       (else
        *unspecified*)))
        
    ;; Pre-unwind handler is not used
    ))

(define (call-with-error-handler thunk)
  "This procedure calls THUNK. On error the error GUI error handler is used.
On traps, the GUI trap handler is used. Will abort to prompt on a
'quit signal."
  (catch
    ;; key
    #t
    ;; thunk
    (with-default-trap-handler
     trap-handler
     (lambda () (start-stack #t thunk)))
    
    ;; On-unwind handler : catch
    (lambda (key . args)
      (cond
       ((eqv? key 'quit)
        (apply throw key args))
       ((eqv? key 'stack-overflow)
        (display "Stack overflow!" %gdn-error-port)
        (newline %gdn-error-port))
       ((eqv? key 'out-of-memory)
        (display "Out of memory!" %gdn-error-port)
        (newline %gdn-error-port))
       (else
        *unspecified*)))
        
    ;; Pre-unwind handler
    error-handler))

(define (print-return-values . return-vals)
  ;; Print the return, which could be multiple values.
  (for-each (lambda (val)
              (unless (eq? val *unspecified*)
                (display "→ ")
                (write val)
                (newline)))
            return-vals))

(define* (trap-handler frame #:optional (trap-idx #f) (trap-name "anonymous"))
  "This trap handler launches Guidance's trap-handler UX."
  (set-vm-trace-level! 0)
  (if trap-idx
      (begin
        (format #t "Trap ~d: ~a~%" trap-idx trap-name)
        (format #t "Entering trap mode.~%"))
      ;; else
      (format #t "Entering break mode.~%"))
  (format #t "When ready, press the CONTINUE button to continue.~%")

  (gdn-set-input-mode! *gdn-application-window* 'trap)
  (gdn-update-backtrace! *gdn-backtrace-view* frame)
      
  (while #t
    (format %gdn-prompt-port "trap ~a>" trap-idx)
        
    (let ((input (gdn-get-user-input *gdn-lisp*)))
      (cond
       ;; The Stop Button returns to eval mode
       ((eqv? (car input) 'stop)
        (break))
       ((eqv? (car input) 'eval)
        (display "-> ")

        (write (print-error-on-exception (eval-string (cadr input))))
        (newline))
       (else
        *unspecified*))))
  (set-vm-trace-level! 1))

(define (error-handler key subr message args rest)
  "This pre-unwind handler launches Guidance's error-handler UX on all
errors except 'quit."
  (unless (eqv? key 'quit)
    (let* ((stack (make-stack #t 4 #t)))
      (display-error
       (stack-ref stack 0)
       (current-error-port)
       subr
       message
       args
       rest)
      
      (format #t "Entering error mode.~%")
      (format #t "When ready, press the STOP 🛑 button to terminate.~%")
      
      ;; (gdn-update-backtrace! *gdn-backtrace-view* (stack-ref stack 0))
      (gdn-set-input-mode! *gdn-application-window* 'error)
      (display "error>" %gdn-prompt-port)
      
      (while #t
        (let ((input (gdn-get-user-input *gdn-lisp*)))
          (cond
           ;; The Stop Button returns to eval mode
           ((eqv? (car input) 'stop)
            (break))
           ((eqv? (car input) 'eval)
            (display "-> ")
            (write (print-error-on-exception (eval-string (cadr input))))
            (newline))
           (else
            *unspecified*))))
      #f)))


#|
(define (gdn-error-handler frame)
  "An error handler"
  (%gdn-enable-error-buttons)
  (%gdn-update-thread-info)
  (%gdn-update-environment-info (gdn-get-environment))
  (%gdn-update-trap-info #f)
  (gdn-update-backtrace frame)
  (display "error>" %gdn-prompt-port)
  
  ;; This is a blocking operation, awaiting a response from the
  ;; operator. This cannot be run in the Gtk main thread.
  (let loop ((response (%gdn-get-error-response)))
    (let ((type (car response))
          (data (cdr response)))
      (cond
       ((eq? type 'continue)
        *unspecified*)
       ((eq? type 'restart)
        (%gdn-disable-error-buttons)
        (abort-to-prompt *start-prompt* "error restart"))
       ((eq? type 'eval)
        (display data (current-output-port))
        (newline (current-output-port))
        (display "=> " (current-output-port))
        (write (false-if-exception (eval-string data)) (current-output-port))
        (newline (current-output-port))
        (loop (%gdn-get-error-response)))))))
|#
