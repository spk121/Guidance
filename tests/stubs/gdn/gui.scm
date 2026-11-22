;;; Copyright 2026 Michael Gran
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; A stand-in for the (gdn gui) module that Guidance defines in C, so
;;; that (gdn repl) can be tested in plain Guile without GTK.
;;;
;;; Operator input comes from a script set with `set-input-script!'.
;;; Each element is either a list (ACTION DATA), or a procedure of no
;;; arguments that returns one; procedures are useful for checking
;;; state at that point.  What the REPL tells the GUI is recorded in an
;;; event log, read with `events'.

(define-module (gdn gui)
  #:use-module (gdn lib)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-43)
  #:export (gdn-get-user-input
            gdn-take-requests!
            gdn-take-stop-request!
            gdn-clear-stop-request!
            gdn-set-input-mode!
            gdn-update-backtrace!
            gdn-update-environments!
            gdn-update-traps!
            gdn-update-threads!
            ;; Test controls
            set-input-script!
            push-request!
            request-stop!
            events
            clear-events!
            last-backtrace))

(define *mutex* (make-mutex))
(define *script* '())
(define *requests* '())
(define *stop-requested* #f)
(define *events* '())
(define *last-backtrace* #f)

(define (log! . event)
  (with-mutex *mutex* (set! *events* (cons event *events*))))

(define (events)
  "The event log, oldest first."
  (with-mutex *mutex* (reverse *events*)))

(define (clear-events!)
  (with-mutex *mutex* (set! *events* '())))

(define (last-backtrace)
  "The vector from gdn-get-backtrace for the most recent backtrace."
  *last-backtrace*)

(define (set-input-script! script)
  (set! *script* script))

(define (push-request! request)
  (with-mutex *mutex* (set! *requests* (append *requests* (list request)))))

(define (request-stop!)
  (set! *stop-requested* #t))

;; Primitives used by (gdn repl)

(define (gdn-get-user-input lisp)
  (when (null? *script*)
    (error "The input script ran out while the REPL wanted input"
           (events)))
  (let* ((item (car *script*))
         (input (if (procedure? item) (item) item)))
    (set! *script* (cdr *script*))
    (log! 'input (car input))
    input))

(define (gdn-take-requests! lisp)
  (with-mutex *mutex*
    (let ((requests *requests*))
      (set! *requests* '())
      requests)))

(define (gdn-take-stop-request! lisp)
  (let ((requested *stop-requested*))
    (set! *stop-requested* #f)
    requested))

(define (gdn-clear-stop-request! lisp)
  (set! *stop-requested* #f))

(define (gdn-set-input-mode! window mode)
  (log! 'mode mode))

(define* (gdn-update-backtrace! view frames #:optional top-frame?)
  (let ((backtrace (gdn-get-backtrace frames top-frame?)))
    ;; Check the shape that the C side asserts.
    (vector-for-each
     (lambda (i entry)
       (unless (and (vector? entry)
                    (= 6 (vector-length entry))
                    (string? (vector-ref entry 0))
                    (string? (vector-ref entry 1))
                    (integer? (vector-ref entry 2))
                    (integer? (vector-ref entry 3))
                    (integer? (vector-ref entry 4))
                    (vector? (vector-ref entry 5)))
         (error "Malformed backtrace entry" entry)))
     backtrace)
    (set! *last-backtrace* backtrace)
    (log! 'backtrace (vector-length backtrace))))

(define (gdn-update-environments! view entries)
  (unless (vector? entries)
    (error "Malformed environment" entries)))

(define (gdn-update-traps! view traps)
  (log! 'traps (length traps)))

(define (gdn-update-threads! view)
  #t)
