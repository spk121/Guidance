;; Closes its output port, and then writes to it, which is an error.
(display "closing")
(newline)
(close-port (current-output-port))
(display "after closing")
