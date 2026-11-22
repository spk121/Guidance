;; Like break.scm, but loaded only by the test of breakpoints set
;; before their code is loaded.
(define (numbers)
  (display "one")
  (newline)
  (display "two")
  (newline)
  (_break_)
  (display "three")
  (newline)
  (display "four")
  (newline))

(numbers)
