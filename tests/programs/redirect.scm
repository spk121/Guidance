;; Breaks while its output is going to a string.  The string must hold
;; only what the program wrote: "AB".
(define (render)
  (display "A")
  (_break_)
  (display "B"))

(define rendered (with-output-to-string render))
(display "= ")
(write rendered)
(newline)
