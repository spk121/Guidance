(define (inner x)
  (car x))

(define (outer x)
  (+ 1 (inner x)))

(outer 5)
(display "not reached")
(newline)
