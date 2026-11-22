;; Times (fib 30), to compare speed under Guidance and plain Guile.
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define start (get-internal-real-time))
(display (fib 30))
(newline)
(format #t "fib-ms: ~a~%"
        (quotient (* 1000 (- (get-internal-real-time) start))
                  internal-time-units-per-second))
