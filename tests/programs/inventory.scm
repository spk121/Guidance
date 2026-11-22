;; Totals an inventory, pausing at the first item that is out of stock.
;; Used for the Kind of variables, and for the screenshot.
(define inventory
  '((apples 12 0.5) (pears 0 0.75) (plums 30 0.2)))

(define (item-value item)
  (let ((count (cadr item))
        (price (caddr item)))
    (when (zero? count)
      (_break_)
      (format #t "~a are out of stock~%" (car item)))
    (* count price)))

(define (total items)
  (if (null? items)
      0
      (+ (item-value (car items))
         (total (cdr items)))))

(display (total inventory))
(newline)
