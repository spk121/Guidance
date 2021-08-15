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
             (ice-9 i18n)
             (srfi srfi-43))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info gathering

(define (gdn-string-starts-with prefix str)
  (let ((len (string-length prefix)))
    (cond
     ((< (string-length str) len)
      #f)
     (else
      (string= prefix str 0 len 0 len)))))


;; This is trying to push the frame and variable info into a compact form.
;; Each vector entry should be
;; - a function-call-like string, e.g. "(func 10)"
;; - the filename
;; - the line in file
;; - the column in line
;; - the number of arguments in this function call
;; - a vector bindings, both arguments and locals
;;
;; It does the trick where it builds it upside-down then reverses at the end
(define (gdn-get-backtrace frame)
  (let loop ((frame frame)
             (backtrace '()))
    (cond
     ((and frame
           (not (eqv? (frame-procedure-name frame) '%start-stack))
           (not (eqv? (frame-procedure-name frame) 'save-module-excursion)))
      ;; Were still in the program's stack.
      ;; Assemble this frame's data then step up the stack.
      (let* ((source (frame-source frame))
             (file (if source (or (source:file source) "") ""))
             (line (if source (source:line-for-user source) 0))
             (col (if source (source:column source) 0)))
        (loop (frame-previous frame)
              (cons (vector 
                     (format #f "~A"
                             (cons (or (frame-procedure-name frame) 'λ)
                                   (frame-arguments frame)))
                     file
                     line
                     col
                     (length (frame-arguments frame))
                     (list->vector (frame-bindings frame)))
                    backtrace))))
     (else
      ;; We've reached the top of the program's frames. Above here
      ;; is Guile's own frames.
      (list->vector (reverse backtrace))))))

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

#|
(define (gdn-get-traps)
  (list->vector
   (map (lambda (trap-id)
          (list trap-id (trap-name trap-id) (trap-enabled? trap-id)))
        (list-traps))))
|#

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
    (vector
     (vector "OS Name" (utsname:sysname un))
     (vector "Node Name" (utsname:nodename un))
     (vector "Release" (utsname:release un))
     (vector "Version" (utsname:version un))
     (vector "Machine" (utsname:machine un))
     (vector "Host Name" (gethostname))
     )))

(define (gdn-get-process-information)
  (vector
   (vector "CWD" (getcwd))
   (vector "file creation umask" (format #f "~5,'0o" (umask)))
   (vector "PID" (number->string (getpid)))
   (vector "Groups" (format #f "~S" (getgroups)))
   (vector "PPID" (number->string (getppid)))
   (vector "UID" (number->string (getuid)))
   (vector "UID Name" (passwd:name (getpwuid (getuid))))
   (vector "GID" (number->string (getgid)))
   (vector "GID Name" (group:name (getgrgid (getgid))))
   (vector "EUID" (number->string (geteuid)))
   (vector "EUID Name" (passwd:name (getpwuid (geteuid))))
   (vector "EGID" (number->string (getegid)))
   (vector "EGID Name" (group:name (getgrgid (getegid))))
   (vector "PGRP" (number->string (getpgrp)))
   (vector "Processor Count" (number->string (total-processor-count)))
   (vector "Processors Used" (number->string (current-processor-count)))
))

(define (gdn-get-environment-variables)
  (let ((EV (sort! (environ) string-ci<?)))
    (vector-map (lambda (entry)
           (let ((split-location (string-index entry #\=)))
             (vector (string-take entry split-location)
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
    (vector
     (vector "Current Time in Epoch" (number->locale-string curtime))
     (vector "Seconds in Epoch" (number->locale-string (+ timeofday-sec (/ timeofday-usec 1000000))))
     (vector "Local Time" (strftime "%c" now-local))
     (vector "Local Day of Year" (number->locale-string (tm:yday now-local)))
     (vector "Daylight Savings" (cond ((= 0 (tm:isdst now-local)) "no")
                                    ((< 0 (tm:isdst now-local)) "yes")
                                    ((> 0 (tm:isdst now-local)) "unknown")))
     (vector "Seconds from GMT" (number->locale-string (tm:gmtoff now-local)))
     (vector "Time Zone" (tm:zone now-local))
     (vector "GMT" (strftime "%c" now-gmt))
     (vector "GMT Day of Year" (number->locale-string (tm:yday now-gmt)))
     (vector "CPU Clock Time" (gdn-time-string (/ (tms:clock now-cpu)
                                                      (exact->inexact internal-time-units-per-second))))
     (vector "CPU Process Time" (gdn-time-string (/ (tms:utime now-cpu)
                                                  (exact->inexact internal-time-units-per-second))))
     (vector "CPU System Time" (gdn-time-string (/ (tms:stime now-cpu)
                                                  (exact->inexact internal-time-units-per-second))))
     (vector "CPU CU Time" (gdn-time-string (tms:cutime now-cpu)))
     (vector "CPU CS Time" (gdn-time-string (tms:cstime now-cpu)))
     (vector "Guile Time Units Per Sec" (number->locale-string internal-time-units-per-second))
     (vector "Guile Real Time" (gdn-time-string (/ (get-internal-real-time) (exact->inexact internal-time-units-per-second))))
     (vector "Guile Run Time" (gdn-time-string (/ (get-internal-run-time)
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
      (vector
       (vector "Login Name" login)
       (vector "User Name" (passwd:name uid-pw))
       (vector "User ID" (number->string (passwd:uid uid-pw)))
       (vector "User Group ID" (number->string (passwd:gid uid-pw)))
       (vector "User Full Name" (passwd:gecos uid-pw))
       (vector "User Shell" (passwd:shell uid-pw))
       (vector "User Home" (passwd:dir uid-pw))
       (vector "Group Name" (group:name gid-gr))
       (vector "Group ID" (number->string (group:gid gid-gr)))
       (vector "Effective User Name" (passwd:name uid-pw))
       (vector "Effective User ID" (number->string (passwd:uid uid-pw)))
       (vector "Effective User Group ID" (number->string (passwd:gid uid-pw)))
       (vector "Effective User Full Name" (passwd:gecos uid-pw))
       (vector "Effective User Shell" (passwd:shell uid-pw))
       (vector "Effective User Home" (passwd:dir uid-pw))
       (vector "Effective Group Name" (group:name egid-gr))
       (vector "Effective Group ID" (number->string (group:gid egid-gr)))))))

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
    (vector 
     (vector "GC Time Taken" (gdn-time-string (/ (assoc-ref x 'gc-time-taken) (exact->inexact internal-time-units-per-second))))
     (vector "Heap Size" (gdn-memory-string (assoc-ref x 'heap-size)))
     (vector "Heap Free Size" (gdn-memory-string (assoc-ref x 'heap-free-size)))
     (vector "Heap Total Allocated" (gdn-memory-string (assoc-ref x 'heap-total-allocated)))
     (vector "Heap Allocated Since GC" (gdn-memory-string (assoc-ref x 'heap-allocated-since-gc)))
     (vector "Protected Objects" (number->string (assoc-ref x 'protected-objects)))
     (vector "GC Events" (number->string (assoc-ref x 'gc-times)))
     ;; (vector "GC Live Object Stats" (gc-live-object-stats))
     )))


;; The return value is a vector
;; Each entry is a 2-element vector that is #(category entries)
;; - category is a string
;; - entries is a vector. Each entry is a 2-element vector that is #(key val)
;;   - key is a string
;;   - val is a string
(define (gdn-get-environment)
  "Lots of info about the current running environment."
  (vector
   (vector "system" (gdn-get-system-information))
   (vector "memory" (gdn-get-gc-stats))
   ;;(vectorize "locale" (gdn-get-locale-information))
   ;;(vectorize "process" (gdn-get-process-information))
    ;;(vectorize "time" (gdn-get-time-entries))
   ;; (vectorize "user" (gdn-get-user-information-entries))
   ;; (vectorize "environment" (gdn-get-environment-variables))
   ))

#|
(define (gdn-get-environment)
  (define (vectorize category lst)
    (list->vector
     (map (lambda (x)
            (cons category x))
          lst)))
  (vectorize "system" (gdn-get-system-information)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points

(define (gdn-run-repl)
  (display "BLAMMO")
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

