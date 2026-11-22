(define (compute a b)
  (let ((sum (+ a b))
        (product (* a b)))
    (_break_)
    (list sum product)))

(display (compute 3 4))
(newline)
