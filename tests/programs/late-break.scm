;; Runs for a while before it breaks, so that the test can press keys
;; that are not meant for the break.
(display "started")
(newline)
(sleep 1)
(_break_)
(display "after the break")
(newline)
