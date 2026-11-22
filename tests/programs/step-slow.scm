;; Lines that take Guile a moment, and stepping far longer.
(use-modules (ice-9 pretty-print))

(define (into-library)
  (_break_)
  ;; Many calls of Guile's own procedures.
  (pretty-print (iota 1500) (%make-void-port "w"))
  (display "= printed")
  (newline))

(define (deep)
  (_break_)
  ;; Guile's map is not tail-recursive: 10000 frames deep.
  (length (map (lambda (i) (number->string i)) (iota 10000)))
  (display "= mapped")
  (newline))
