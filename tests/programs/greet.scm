;; Run more than once, to test that a trap at `greet' goes on working
;; when it is defined again.
(define (greet name)
  (string-append "Hello, " name))

(display (greet "world"))
(newline)
