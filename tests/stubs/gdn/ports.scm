;;; Copyright 2026 Michael Gran
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; A stand-in for the (gdn ports) module that Guidance defines in C.
;;; The prompt port collects the prompts the REPL shows.

(define-module (gdn ports)
  #:export (%gdn-input-port
            %gdn-output-port
            %gdn-error-port
            %gdn-prompt-port
            prompts))

(define %gdn-input-port (current-input-port))
(define %gdn-output-port (current-output-port))
(define %gdn-error-port (current-error-port))

(define *prompts* '())
(define %gdn-prompt-port
  (make-soft-port
   (vector (lambda (c) (set! *prompts* (cons (string c) *prompts*)))
           (lambda (s) (set! *prompts* (cons s *prompts*)))
           #f #f #f)
   "w"))

(define (prompts)
  "All prompts shown so far, concatenated."
  (apply string-append (reverse *prompts*)))
