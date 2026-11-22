;; `visit' is called by `map', which is in Guile's library, and breaks
;; in its first call.
(define first-call? #t)

(define (visit x)
  (when first-call?
    (set! first-call? #f)
    (_break_))
  (* x 2))

(define (visit-all)
  (let ((doubles (map visit '(1 2))))
    (display doubles)
    (newline)))

(visit-all)
(display "end")
(newline)
