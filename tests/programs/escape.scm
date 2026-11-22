;; Leaves a loop through a continuation.  Prints 3 on a line of its
;; own.
(define (go)
  (_break_)
  ;; The continuation's value goes straight to `display'.
  (display (call/cc
            (lambda (return)
              (for-each (lambda (x)
                          (when (> x 2)
                            (return x)))
                        '(1 2 3 4))
              'none)))
  (newline)
  (display "done")
  (newline))

(go)
