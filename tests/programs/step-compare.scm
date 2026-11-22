;; Many features of the language, each printing a line that begins
;; with "= ".  The lines must be the same however the program is run:
;; straight through, or under any kind of step.
(use-modules (srfi srfi-1) (srfi srfi-9) (srfi srfi-11)
             (ice-9 receive) (ice-9 match) (ice-9 optargs))

;; A line is written in one piece, so that a message from Guidance,
;; when the program pauses, cannot come in the middle of it.
(define (show label value)
  (display (string-append "= " label " " (object->string value) "\n")))

(define* (opt a #:optional (b 2) #:key (c 3) #:rest r)
  (list a b c r))

(define cl
  (case-lambda
    ((a) (list 'one a))
    ((a b) (list 'two a b))
    ((a . r) (list 'many a r))))

(define (two x)
  (values x (* x 2)))

(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y set-point-y!))

(define p (make-parameter 10))
(define fl (make-fluid 5))

(define (gen)
  (call-with-prompt 'g
    (lambda () (+ 1 (abort-to-prompt 'g 'yielded)))
    (lambda (k v) (cons v k))))

(define (go)
  (_break_)
  (show "opt" (list (opt 1) (opt 1 5) (opt 1 5 #:c 7)))
  (show "case-lambda" (list (cl 1) (cl 1 2) (cl 1 2 3)))
  (show "values" (call-with-values (lambda () (two 4)) list))
  (let-values (((a b) (two 6)) ((c) (values 9)))
    (show "let-values" (list a b c)))
  (receive (q . rest) (values 1 2 3)
    (show "receive" (list q rest)))
  (show "apply" (apply + 1 2 (list 3 4)))
  (show "record" (let ((pt (make-point 1 2)))
                   (set-point-y! pt 5)
                   (list (point-x pt) (point-y pt) (point? pt))))
  (show "parameterize" (list (p) (parameterize ((p 20)) (p)) (p)))
  (show "fluid" (list (fluid-ref fl) (with-fluids ((fl 6)) (fluid-ref fl))))
  (show "catch" (catch 'boom
                  (lambda () (throw 'boom 1 2))
                  (lambda (key . args) (cons key args))))
  (show "false-if-exception" (false-if-exception (car 5)))
  (show "dynamic-wind"
        (let ((log '()))
          (dynamic-wind
            (lambda () (set! log (cons 'in log)))
            (lambda () (set! log (cons 'body log)))
            (lambda () (set! log (cons 'out log))))
          (reverse log)))
  (let ((r (gen)))
    (show "prompt-yield" (car r))
    (show "prompt-resume" ((cdr r) 41)))
  (show "sort" (sort (list 3 1 2) (lambda (a b) (< a b))))
  (show "fold" (fold (lambda (x acc) (+ x acc)) 0 (iota 10)))
  (show "match" (match '(1 (2 3)) ((a (b c)) (list c b a))))
  (show "named-let" (let loop ((i 0) (acc '()))
                      (if (< i 4) (loop (1+ i) (cons i acc)) acc)))
  (show "hash" (let ((h (make-hash-table)))
                 (hash-set! h 'a 1)
                 (hash-ref h 'a)))
  (show "do" (do ((i 0 (1+ i)) (s 0 (+ s i))) ((= i 5) s)))
  (show "delay" (force (delay (* 6 7)))))

(go)
(show "end" 'reached)
