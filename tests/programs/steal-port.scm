;; Changes the current output port, and does not put it back.
(display "= before")
(newline)
(set-current-output-port (open-output-string))
(display "hidden")
