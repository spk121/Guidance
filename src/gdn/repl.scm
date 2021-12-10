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
(define-module (gdn repl)
  #:use-module (gdn lib)
  #:use-module (gdn gui)
  #:use-module (gdn ports)
  #:use-module (gdn widgets)
  #:use-module (ice-9 control)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 eval-string)
  #:use-module (system vm vm)
  #:use-module (system vm traps)
  #:use-module (system vm trap-state)
  #:export (gdn-top-repl _break_)
  #:declarative? #f)

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

(define (abort-on-error exp)
  (catch #t
    (lambda () exp)
    (lambda (key subr message args rest)
      (format #t "While ~A:~%" string)
      (display-error #f
                     (current-error-port)
                     subr
                     message
                     args
                     rest)
      (abort))))


(define (gdn-top-repl)
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
      (gdn-update-threads! *gdn-thread-view*)
      (gdn-get-user-input *gdn-lisp*))
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
         *unspecified*)))))

(define (run-repl repl)
  (run-repl* repl repl-read))

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
           ;; (pk 'nu action data)
           (cond
            ((eqv? action #f))               ; unknown action: ignore
            ((eqv? action 'quit)
             (abort '()))
            ((eqv? action 'run)
             (if (not (= (length *cmdline*) 2))
                 (format #t "No file given on command line~%")
                 ;; else
                 (let ((thunk
                        (lambda ()
                          (load (cadr *cmdline*)))))
                   (call-with-error-handler thunk))))
            (else
             ;; (pk 'about-to-call-execute-input)
             (call-with-minimal-error-handler
              (lambda ()
                ;; (pk 'alpha)
                (catch 'quit
                  (lambda ()
                    ;; (pk 'bravo)
                    (call-with-values
                        (lambda ()
                          ;; (pk 'charlie)
                          #;(%
                           ;; prompt expression
                           (let ((thunk
                                  (abort-on-error
                                   "compiling expression"
                                   (eval-string data #:compile? #t))))
                             (pk 'delta 'thunk thunk)
                             (call-with-error-handler
                              (lambda ()
                                (with-stack-and-prompt thunk))))
                           
                           ;; prompt handler
                           (lambda (k)
                          (values)))

                          (let ((thunk
                                 (lambda ()
                                   (eval-string data #:compile? #t)
                                   )))
                            (call-with-error-handler thunk)))
                           
                      
                      print-return-values))

                  ;; The handler for 'quit
                  (lambda (k . args)
                    (abort args)))))))))
                
     ;; Handler for the outer '%' prompt
     (lambda (k . status)
       status)))))

(define (execute-input-and-print-return-values action data)
  (call-with-values
      (lambda ()
        (%
         (execute-input action data)))
    print-return-values))
  
(define (execute-input action data)
  (call-with-error-handler
   (lambda ()
     (cond
      ((eqv? action 'run)
       (pk 'omicron action data)
       ;; FIXME: insert with-stack-and-prompt here?
       (eval (compile-shell-switches *cmdline*)
             (interaction-envirnoment))
       ;; (error "not supposed to be here")
       )
      
      ((eqv? action 'eval)
       ;; (pk 'in-eval action data)
       (with-stack-and-prompt
        (lambda ()
          (eval-string data #:compile? #t))))
      (else
       *unspecified*)))))


(define (call-with-minimal-error-handler thunk)
  "This procedure calls THUNK. On some critical errors, it prints a brief
error message. Traps are disabled."
  (catch
    ;; key
    #t
    ;; thunk
    (lambda ()
      (with-default-trap-handler
       ;; Use a null trap handler to ignore traps.
       (lambda (frame trap-idx trap-name)
         (format #t "Trap ~A ignored~%" trap-name)
         #t)
       (start-stack #t thunk)))
    
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
        
    ;; Pre-unwind handler is not used
    ))

(define (call-with-error-handler thunk)
  "This procedure calls THUNK. On error the error GUI error handler is used.
On traps, the GUI trap handler is used. Will abort to prompt on a
'quit signal."
  ;; (pk 'foxtrot)
  (catch
    ;; key
    #t
    ;; thunk
    (with-default-trap-handler
     trap-handler
     (lambda () (start-stack #t thunk))
     )
    
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

(define (_break_)
  (display "in _break_ \n")
  (let ((frame (stack-ref (make-stack #t 1) 0)))
    (add-ephemeral-trap-at-frame-finish! frame trap-handler)))
;;  (trap-handler (stack-ref (make-stack #t) 6)))

(define* (trap-handler frame #:optional (trap-idx #f) (trap-name "break"))
  "This trap handler launches Guidance's trap-handler UX."
  (gdn-set-input-mode! *gdn-application-window* 'trap)
  ;; (pk 'lima frame trap-idx trap-name)
  (set-vm-trace-level! 0)
  (if trap-idx
      (begin
        (format #t "Trap ~a: ~a~%" trap-idx trap-name)
        (format #t "Entering trap mode.~%"))
      ;; else
      (format #t "Entering break mode.~%"))
  (format #t "When ready, press the RUN button to continue.~%")

  (gdn-update-backtrace! *gdn-backtrace-view* frame)
      
  (while #t
    (format %gdn-prompt-port "trap ~a>" trap-idx)
    (gdn-update-environments! *gdn-environment-view* (gdn-get-environment))
    (gdn-update-traps! *gdn-trap-view* (list-traps))
    (gdn-update-threads! *gdn-thread-view*)
        
    (let* ((input (gdn-get-user-input *gdn-lisp*))
           (action (car input))
           (data (cadr input)))
      (cond
       ;; The Stop Button returns to eval mode
       ((eqv? action 'run)
        (break))
       
       ((eqv? action 'step)
        (add-ephemeral-stepping-trap! frame trap-handler #:into? #f #:instruction? #f)
        (gdn-update-traps! *gdn-trap-view* (list-traps))
        (break))
       
       ((eqv? action 'step-instruction)
        (add-ephemeral-stepping-trap! frame trap-handler #:into? #f #:instruction? #t)
        (gdn-update-traps! *gdn-trap-view* (list-traps))
        (break))
       
       ((eqv? action 'step-into)
        (add-ephemeral-stepping-trap! frame trap-handler #:into? #t #:instruction? #f)
        (gdn-update-traps! *gdn-trap-view* (list-traps))
        (break))
       
       ((eqv? action 'step-into-instruction)
        (add-ephemeral-stepping-trap! frame trap-handler #:into? #t #:instruction? #t)
        (break))
       
       ((eqv? action 'step-out)
        (add-ephemeral-trap-at-frame-finish! frame trap-handler)
        (gdn-update-traps! *gdn-trap-view* (list-traps))
        (break))

       ((eqv? action 'eval)
        (let ((out (print-error-on-exception
                    (lambda () (eval-string data)))))
          (unless (unspecified? out)
            (display "trap> ")
            (write out)
            (newline))))
       
       (else
        *unspecified*))))
  (set-vm-trace-level! 1))

(define (error-handler key subr message args rest)
  "This pre-unwind handler launches Guidance's error-handler UX on all
errors except 'quit."
  (gdn-set-input-mode! *gdn-application-window* 'error)
  ;; (pk 'india key subr message args rest)
  (backtrace)
  (unless (eqv? key 'quit)
    ;; (pk 'juliette)
    (let* ((stack (make-stack #t 3)))
      ;; (pk 'juilette stack)
      (display-error
       #f ;;(stack-ref stack 0)
       (current-error-port)
       subr
       message
       args
       rest)
      
      (format #t "Entering error mode.~%")
      (format #t "When ready, press the STOP 🛑 button to terminate.~%")
      
      (gdn-update-backtrace! *gdn-backtrace-view* (stack-ref stack 0))
      (gdn-update-environments! *gdn-environment-view* (gdn-get-environment))
      (gdn-update-traps! *gdn-trap-view* (list-traps))
      (gdn-update-threads! *gdn-thread-view*)
      
      (display "error>" %gdn-prompt-port)
      
      (while #t
        ;; (pk 'mike)
        (sleep 1)
        (let* ((input (gdn-get-user-input *gdn-lisp*))
               (action (car input))
               (data (cadr input)))
          ;; (pk 'kilo input)
          (cond
           ;; The Stop Button returns to eval mode
           ((eqv? action 'stop)
            (break))
           ((eqv? action 'eval)
            (let ((out (print-error-on-exception
                        (lambda () (eval-string data)))))
              (unless (unspecified? out)
                (display "err> ")
                (write out)
                (newline))))
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
