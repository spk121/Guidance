;; Reads lines until the end of its input.
(use-modules (ice-9 rdelim))

(display "Type lines, then Ctrl+D:")
(newline)
(let loop ((lines '()))
  (let ((line (read-line)))
    (if (eof-object? line)
        (format #t "end of input after ~s~%" (reverse lines))
        (loop (cons line lines)))))
