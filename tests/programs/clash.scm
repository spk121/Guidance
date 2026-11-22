;; An ordinary program, some of whose names are also names inside
;; Guidance.
(define (option key) (list 'my-option key))
(define (program-name) "my program")
(define (pause seconds)
  (display "pausing ")
  (display seconds)
  (newline))

(display (option 'verbose))
(newline)
(pause 1)
(display "clash program done")
(newline)
