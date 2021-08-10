;;; Copyright 2021 Michael Gran
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(use-modules (system repl repl)
             (system repl debug)
             (system vm frame)
             (system vm program)
             (system vm trap-state)
             (ice-9 command-line)
             (ice-9 top-repl)
             (ice-9 pretty-print)
             (ice-9 i18n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info gathering

(define (gdn-stringify-var x)
  (cond
   ((unspecified? x)
    "unspecified")
   ((procedure? x)
    (format #f "procedure: ~a" (or (procedure-name x) #\λ)))
   ((variable? x)
    (format #f "variable: ~a" (gdn-stringify-var (variable-ref x))))
   (else
    (format #f "~a" x))))

(define (gdn-unpack-frame-bindings bindings max-index)
  (if (null? bindings)
      '()
      ;; else
      (let loop ((cur (car bindings))
                 (rest (cdr bindings))
                 (output '()))
        (let ((binding-info
               (list (if (> (binding-index cur) max-index)
                         'local
                         'arg)
                     (binding-name cur)
                     ;; (binding-representation cur)
                     (gdn-stringify-var (binding-ref cur)))))
          (if (null? rest)
              (reverse (cons binding-info output))
              ;; else
              (loop (car rest)
                    (cdr rest)
                    (cons binding-info output)))))))

(define (gdn-get-backtrace frame)
  (let loop ((frame frame)
             (backtrace '()))
    (if (or (not frame)
            (eqv? (frame-procedure-name frame) '%start-stack)
            (eqv? (frame-procedure-name frame) 'save-module-excursion))
        (reverse backtrace)
        ;; else
        (let* ((source (frame-source frame))
               (file (and=> source source:file))
               (line (and=> source source:line-for-user))
               (col (and=> source source:column)))
          (loop (frame-previous frame)
                (cons (list (append (list (or (frame-procedure-name frame)
                                              'λ))
                                    (frame-arguments frame))
                            (list file line col)
                            (gdn-unpack-frame-bindings (frame-bindings frame)
                                                       (length (frame-arguments frame)))
                            )
                      backtrace))))))
#|
(define (funk x)
  (pretty-print
   (gdn-get-backtrace
    (vector-ref (stack->vector (make-stack #t 1)) 0)))
  (newline)
  x)

(define (func y)
  (funk (1+ y)))
|#

(define (gdn-get-threads)
  (list
   (current-thread)
   (map (lambda (thrd)
          (list thrd (not (thread-exited? thrd))))
        (all-threads))))

(define (gdn-get-traps)
   (map (lambda (trap-id)
          (list trap-id (trap-name trap-id) (trap-enabled? trap-id)))
        (list-traps)))

(define (gdn-get-locale-information)
  (list
   (list "LC_ALL" (setlocale LC_ALL))
   (list "LC_COLLATE" (setlocale LC_COLLATE))
   (list "LC_CTYPE" (setlocale LC_CTYPE))
   (list "LC_MESSAGES" (setlocale LC_MESSAGES))
   (list "LC_MONETARY" (setlocale LC_MONETARY))
   (list "LC_NUMERIC" (setlocale LC_NUMERIC))
   (list "LC_TIME" (setlocale LC_TIME))))

(define (gdn-get-system-information)
  (let ((un (uname)))
    (list
     (list "OS Name" (utsname:sysname un))
     (list "Node Name" (utsname:nodename un))
     (list "Release" (utsname:release un))
     (list "Version" (utsname:version un))
     (list "Machine" (utsname:machine un))
     (list "Host Name" (gethostname))
     )))

(define (gdn-get-process-information)
  (list
   (list "CWD" (getcwd))
   (list "file creation umask" (format #f "~5,'0o" (umask)))
   (list "PID" (number->string (getpid)))
   (list "Groups" (format #f "~S" (getgroups)))
   (list "PPID" (number->string (getppid)))
   (list "UID" (number->string (getuid)))
   (list "UID Name" (passwd:name (getpwuid (getuid))))
   (list "GID" (number->string (getgid)))
   (list "GID Name" (group:name (getgrgid (getgid))))
   (list "EUID" (number->string (geteuid)))
   (list "EUID Name" (passwd:name (getpwuid (geteuid))))
   (list "EGID" (number->string (getegid)))
   (list "EGID Name" (group:name (getgrgid (getegid))))
   (list "PGRP" (number->string (getpgrp)))
   (list "Processor Count" (number->string (total-processor-count)))
   (list "Processors Used" (number->string (current-processor-count)))
))

(define (gdn-get-environment-variables)
  (let ((EV (sort! (environ) string-ci<?)))
    (map (lambda (entry)
           (let ((split-location (string-index entry #\=)))
             (list (string-take entry split-location)
                   (string-drop entry (1+ split-location)))))
         EV)))

(define (gdn-get-time-entries)
  (let* ((curtime (current-time))
         (timeofday (gettimeofday))
         (timeofday-sec (car timeofday))
         (timeofday-usec (cdr timeofday))
         (now-local (localtime curtime))
         (now-gmt (gmtime curtime))
         (now-cpu (times)))
    (list
     (list "Current Time in Epoch" (number->locale-string curtime))
     (list "Seconds in Epoch" (number->locale-string (+ timeofday-sec (/ timeofday-usec 1000000))))
     (list "Local Time" (strftime "%c" now-local))
     (list "Local Day of Year" (number->locale-string (tm:yday now-local)))
     (list "Daylight Savings" (cond ((= 0 (tm:isdst now-local)) "no")
                                    ((< 0 (tm:isdst now-local)) "yes")
                                    ((> 0 (tm:isdst now-local)) "unknown")))
     (list "Seconds from GMT" (number->locale-string (tm:gmtoff now-local)))
     (list "Time Zone" (tm:zone now-local))
     (list "GMT" (strftime "%c" now-gmt))
     (list "GMT Day of Year" (number->locale-string (tm:yday now-gmt)))
     (list "CPU Clock Time" (gdn-time-string (/ (tms:clock now-cpu)
                                                      (exact->inexact internal-time-units-per-second))))
     (list "CPU Process Time" (gdn-time-string (/ (tms:utime now-cpu)
                                                  (exact->inexact internal-time-units-per-second))))
     (list "CPU System Time" (gdn-time-string (/ (tms:stime now-cpu)
                                                  (exact->inexact internal-time-units-per-second))))
     (list "CPU CU Time" (gdn-time-string (tms:cutime now-cpu)))
     (list "CPU CS Time" (gdn-time-string (tms:cstime now-cpu)))
     (list "Guile Time Units Per Sec" (number->locale-string internal-time-units-per-second))
     (list "Guile Real Time" (gdn-time-string (/ (get-internal-real-time) (exact->inexact internal-time-units-per-second))))
     (list "Guile Run Time" (gdn-time-string (/ (get-internal-run-time)
                                                (exact->inexact internal-time-units-per-second)))))))

(define (gdn-get-user-information-entries)
  (let ((uid (getuid))
        (euid (geteuid))
        (gid (getgid))
        (egid (getegid))
        (login (getlogin)))
    (let ((uid-pw (getpw uid))
          (euid-pw (getpw euid))
          (gid-gr (getgr gid))
          (egid-gr (getgr egid)))
      (list
       (list "Login Name" login)
       (list "User Name" (passwd:name uid-pw))
       (list "User ID" (number->string (passwd:uid uid-pw)))
       (list "User Group ID" (number->string (passwd:gid uid-pw)))
       (list "User Full Name" (passwd:gecos uid-pw))
       (list "User Shell" (passwd:shell uid-pw))
       (list "User Home" (passwd:dir uid-pw))
       (list "Group Name" (group:name gid-gr))
       (list "Group ID" (number->string (group:gid gid-gr)))
       (list "Effective User Name" (passwd:name uid-pw))
       (list "Effective User ID" (number->string (passwd:uid uid-pw)))
       (list "Effective User Group ID" (number->string (passwd:gid uid-pw)))
       (list "Effective User Full Name" (passwd:gecos uid-pw))
       (list "Effective User Shell" (passwd:shell uid-pw))
       (list "Effective User Home" (passwd:dir uid-pw))
       (list "Effective Group Name" (group:name egid-gr))
       (list "Effective Group ID" (number->string (group:gid egid-gr)))))))

(define (gdn-memory-string x)
  (cond
   ((< x 1000)
    (format #f "~d B" x))
   ((< x 1000000)
    (format #f "~4f kB" (/ x 1000)))
   ((< x 1000000000)
    (format #f "~4f MB" (/ x 1000000)))
   ((< x 1000000000000)
    (format #f "~4f GB" (/ x 1000000000)))
   (else
    (format #f "~4f TB" (/ x 1000000000000)))))

(define (gdn-time-string x)
  (cond
   ((< x 0.001)
    (format #f "~5f µs" (* x 1000000)))
   ((< x 1)
    (format #f "~5f ms" (* x 1000)))
   ((< x 60)
    (format #f "~4f sec" x))
   ((< x 3600)
    (format #f "~4f min" (/ x 60)))
   ((< x (* 24 3600))
    (format #f "~4f hr" (/ x 3600)))
   (else
    (format #f "~4f days" (/ x (* 24 3600))))))

(define (gdn-get-gc-stats)
  (let ((x (gc-stats)))
    (list 
          (list "GC Time Taken" (gdn-time-string (/ (assoc-ref x 'gc-time-taken) (exact->inexact internal-time-units-per-second))))
          (list "Heap Size" (gdn-memory-string (assoc-ref x 'heap-size)))
          (list "Heap Free Size" (gdn-memory-string (assoc-ref x 'heap-free-size)))
          (list "Heap Total Allocated" (gdn-memory-string (assoc-ref x 'heap-total-allocated)))
          (list "Heap Allocated Since GC" (gdn-memory-string (assoc-ref x 'heap-allocated-since-gc)))
          (list "Protected Objects" (assoc-ref x 'protected-objects))
          (list "GC Events" (assoc-ref x 'gc-times))
        ;; (list "GC Live Object Stats" (gc-live-object-stats))
          )))

(define (gdn-get-environment)
  (list (cons 'system (gdn-get-system-information))
        (cons 'gc (gdn-get-gc-stats))
        (cons 'locale (gdn-get-locale-information))
        ;; (get-tty-information)
        (cons 'process (gdn-get-process-information))
        (cons 'time (gdn-get-time-entries))
        (cons 'user (get-user-information-entries))
        (cons 'environment (get-environment-variables))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points

(define (gdn-run-repl)
  (top-repl))

(define (gdn-run-argv argv)
  "Process the command line argument list and then run a Guile
interpreter."
  (eval (compile-shell-switches argv) (current-module)))

(define run-trap (add-trap-at-procedure-call! gdn-run-argv))

(define (gdn-run-trap-enable)
  (enable-trap! run-trap))

(define (gdn-run-trap-disable)
  (disable-trap! run-trap))
