;;; Copyright 2026 Michael Gran
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; A stand-in for the (gdn widgets) module that Guidance defines in C.

(define-module (gdn widgets)
  #:export (*gdn-application-window*
            *gdn-backtrace-view*
            *gdn-environment-view*
            *gdn-source-view*
            *gdn-thread-view*
            *gdn-trap-view*
            *gdn-module-view*
            *gdn-lisp*))

(define *gdn-application-window* 'application-window)
(define *gdn-backtrace-view* 'backtrace-view)
(define *gdn-environment-view* 'environment-view)
(define *gdn-source-view* 'source-view)
(define *gdn-thread-view* 'thread-view)
(define *gdn-trap-view* 'trap-view)
(define *gdn-module-view* 'module-view)
(define *gdn-lisp* 'lisp)
