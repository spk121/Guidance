;; Prints more than the terminal keeps, as fast as it can, in lines
;; with characters of several bytes, which reads can split.
(let loop ((i 1))
  (when (<= i 30000)
    (display "line ")
    (display i)
    (display " λ→日本語 äöü")
    (newline)
    (loop (1+ i))))
(display "end of flood")
(newline)
