(define (tick n)
  (1+ n))

(define (spin n)
  (spin (tick n)))

(display "spinning")
(newline)
(spin 0)
