;; Starts a thread that counts for three seconds, then spins until it
;; is stopped.
(use-modules (ice-9 threads))

(define counter 0)

(define worker
  (call-with-new-thread
   (lambda ()
     (let loop ((i 0))
       (when (< i 150)
         (usleep 20000)
         (set! counter (1+ counter))
         (loop (1+ i)))))))

(display "= spinning")
(newline)
(let loop ()
  (usleep 10000)
  (loop))
