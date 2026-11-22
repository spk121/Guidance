;; A module found through -L, for the command-line tests.
(define-module (guidance-test greeting)
  #:export (greeting))

(define (greeting name)
  (string-append "Hello, " name))
