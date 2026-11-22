;; Stepping over the call to `bad' raises an error while the step is
;; still pending.
(define (bad x)
  (car x))

(define (go)
  (_break_)
  (display "before")
  (newline)
  (bad 5)
  (display "not reached")
  (newline))

(go)
