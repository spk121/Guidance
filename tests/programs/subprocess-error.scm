;; Subprocesses that write to their error streams: one started without
;; a shell, and one on a pipe.
(use-modules (ice-9 popen) (ice-9 rdelim))

(system* "sh" "-c" "echo from-the-error-stream >&2")

(let ((pipe (open-input-pipe "echo error-of-a-pipe >&2; echo piped")))
  (display (list 'read (read-line pipe)))
  (newline)
  (close-pipe pipe))

(display "subprocess finished")
(newline)
