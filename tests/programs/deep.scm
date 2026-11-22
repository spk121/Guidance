;; Recurses without end.
(define (deep n)
  (+ 1 (deep (+ n 1))))

(deep 0)
