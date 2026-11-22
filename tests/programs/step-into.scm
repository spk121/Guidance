;; Step Into through code that throws and catches, and that loads a
;; file.  Guidance used to corrupt the program doing this.
(define (go)
  (_break_)
  (display (catch #t (lambda () (throw 'k 1)) (lambda args args)))
  (newline)
  (load "step-into-other.scm")
  (display (list 'other other-value))
  (newline)
  (display "done")
  (newline))

(go)
