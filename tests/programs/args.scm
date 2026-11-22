;; Shows its command line, and uses a module found through -L.
(use-modules (guidance-test greeting))

(define (main args)
  (display (list 'main args))
  (newline))

(display (list 'args (command-line)))
(newline)
(display (greeting "world"))
(newline)
