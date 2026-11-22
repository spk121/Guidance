;; A module found through -L, for the command-line tests.
(define-module (guidance-test greeting)
  #:export (greeting greet-arguments))

(define (greeting name)
  (string-append "Hello, " name))

;; For -e (@ (guidance-test greeting) greet-arguments).
(define (greet-arguments args)
  (display (list 'greet-arguments args))
  (newline))
