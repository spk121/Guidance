;; Output and error output, interleaved, for the order in which the
;; Terminal shows them.  The last line has an escape sequence of its
;; own, which the Terminal should not show.
(let loop ((i 1))
  (when (<= i 5)
    (format #t "out ~a~%" i)
    (format (current-error-port) "err ~a~%" i)
    (loop (1+ i))))
(display "plain \x1b[1mbold\x1b[0m plain")
(newline)
