;; Reads a line from standard input.
(use-modules (ice-9 rdelim))

(display "What is your name?")
(newline)
(display (string-append "Hello, " (read-line) "!"))
(newline)
