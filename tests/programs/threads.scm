;; Errors and _break_ in a thread other than the program's main one.
(use-modules (ice-9 threads))

(join-thread (call-with-new-thread (lambda () (car 5))))
(display "joined the failed thread")
(newline)

(join-thread (call-with-new-thread
              (lambda ()
                (_break_)
                (display "after _break_ in a thread")
                (newline))))

;; A thread with a handler of its own keeps it.
(display (join-thread (call-with-new-thread
                       (lambda () (throw 'oops))
                       (lambda (key . args) (list 'handled key)))))
(newline)
(display "end")
(newline)
