;; Subprocesses started with `system', which inherit file descriptors
;; 0 to 2 rather than using Guile's ports.
(system "echo output from a subprocess")
(system "echo errors from a subprocess >&2")
(display "Type a line for a subprocess:")
(newline)
(system "read line; echo the subprocess read: $line")
