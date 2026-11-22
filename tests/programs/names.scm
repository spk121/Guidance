;; Guile's optimizer would inline `helper' into `work', and lose the
;; name of `a'.
(define (helper x)
  (let ((y (* x 2)))
    (+ y 1)))

(define (work)
  (_break_)
  (let ((a (helper 3)))
    (display a)
    (newline)
    (display (* a 2))
    (newline)
    a))

(work)
(display "done")
(newline)
