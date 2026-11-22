;; Threads for the Threads page: one that has finished, and one that
;; is still running when the program pauses.
(use-modules (ice-9 threads))

(define (finisher) 'done)
(define ports (pipe))
(define (reader) (read-char (car ports)))

(join-thread (call-with-new-thread finisher))
(define waiting (call-with-new-thread reader))
(_break_)
(close-port (cdr ports))
(join-thread waiting)
(display "threads done")
(newline)
