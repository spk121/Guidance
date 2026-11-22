;; Calls a continuation that it has already returned through, twice.
;; Prints 1, 10 and 20, each on a line of its own.
(define k #f)
(define n 0)

(define (go)
  (_break_)
  ;; The continuation's value goes straight to `display'.
  (display (call/cc (lambda (c) (set! k c) 1)))
  (newline)
  (set! n (1+ n))
  (when (< n 3)
    (k (* n 10)))
  (display "done")
  (newline))

(go)
