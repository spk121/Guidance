;; A subprocess, started without a shell, that writes to its error
;; stream.
(system* "sh" "-c" "echo from-the-error-stream >&2")
(display "subprocess finished")
(newline)
