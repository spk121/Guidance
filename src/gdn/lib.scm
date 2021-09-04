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
(use-modules
 ((system repl debug) #:select (stack->vector) #:prefix __)
 ((system vm frame) #:select (frame-bindings binding-representation binding-index binding-ref binding-name) #:prefix __)
 ((system vm program) #:select (source:file source:column source:line-for-user) #:prefix __)
 ((system vm trap-state) #:select (add-trap-at-procedure-call! disable-trap! enable-trap! install-trap-handler!) #:prefix __)
 (system repl error-handling)
 ((ice-9 command-line) #:select (compile-shell-switches) #:prefix __)
 ((ice-9 top-repl) #:select (top-repl) #:prefix __)
 ((ice-9 i18n) #:select (number->locale-string) #:prefix __)
 (ice-9 rdelim)
 ((ice-9 threads) #:select()))

(define (gdn-string-starts-with prefix str)
  (let ((len (string-length prefix)))
    (cond
     ((< (string-length str) len)
      #f)
     (else
      (string= prefix str 0 len 0 len)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKTRACE

(define (gdn-value->string x)
  (format #f "~A" x))

(define (gdn-value->extra-info x)
  (cond
   ;; ((procedure? x)
   ;;  (or (procedure-name x) "λ"))
   ;; ((and (variable? x) (variable-bound? x))
   ;;  (gdn-value->string (variable-ref x)))
   ;; ((and (variable? x) (not (variable-bound? x)))
   ;;  "undefined")
   ((fluid? x)
    (gdn-value->string (or (false-if-exception (fluid-ref x)) "unbound")))
   (else
    "")))

(define (gdn-unpack-binding n_args binding)
  (let ((name (__binding-name binding))
        (val (__binding-ref binding)))
    (vector
     (if name
         (symbol->string name)
         "(anonymous)")
     (if (< (__binding-index binding) n_args)
         #t
         #f)
     (gdn-value->string (__binding-representation binding))
     (gdn-value->string val)
     (gdn-value->extra-info val))))

(define (gdn-format-frame-arguments frame)
  (with-output-to-string
    (lambda ()
      (display "(")
      (display (or (frame-procedure-name frame) 'λ))
      (for-each
       (lambda (arg)
         (newline)
         (display "  ")
         (display arg))
       (frame-arguments frame))
      (display ")"))))

;; This is trying to push the frame and variable info into a compact form.
;; Each vector entry should be
;; - call (string): a function-call-like string, e.g. "(func 10)"
;; - filename (string)
;; - line (integer): the line number in file
;; - column (integer): the column number
;; - nargs (integer): the number of arguments in this function call
;; - bindings (vector): a vector bindings, both arguments and locals
;;
;; The bindings themselves are each a vector
;; - name (string)
;; - argument (boolean)
;; - representation (string)
;; - value (string)
;; - extra (string)
;;
;; It does the trick where it builds it upside-down then reverses at
;; the end.
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
             (file (if source (or (__source:file source) "") ""))
             (line (if source (__source:line-for-user source) 0))
             (col (if source (__source:column source) 0)))
        (loop (frame-previous frame)
              (cons (vector
                     (gdn-format-frame-arguments frame)
                     file
                     line
                     col
                     (length (frame-arguments frame))
                     (list->vector (map (lambda (binding)
                                          (gdn-unpack-binding (length (frame-arguments frame)) binding))
                                        (__frame-bindings frame))))
                    backtrace))))
     (else
      ;; We've reached the top of the program's frames. Above here
      ;; is Guile's own frames.
      (list->vector (reverse backtrace))))))

(define (gdn-test-backtrace x)
  (write (gdn-get-backtrace
          (vector-ref (__stack->vector (make-stack #t 1)) 0)))
  (newline)
  x)

#|
(define (func y)
(funk (1+ y)))
|#

#|
(define (get-traps)
(list->vector
(map (lambda (trap-id)
(list trap-id (trap-name trap-id) (trap-enabled? trap-id)))
(list-traps))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT INFO
(define (gdn-memory-string x)
  (cond
   ((< x 1000)
    (format #f "~d B" x))
   ((< x 1000000)
    (format #f "~5f kB" (/ x 1000)))
   ((< x 1000000000)
    (format #f "~5f MB" (/ x 1000000)))
   ((< x 1000000000000)
    (format #f "~5f GB" (/ x 1000000000)))
   (else
    (format #f "~5f TB" (/ x 1000000000000)))))

(define (gdn-time-string x)
  (cond
   ((< x 0.001)
    (format #f "~5f µs" (* x 1000000)))
   ((< x 1)
    (format #f "~5f ms" (* x 1000)))
   ((< x 60)
    (format #f "~4f s" x))
   ((< x 3600)
    (format #f "~4f m" (/ x 60)))
   ((< x (* 24 3600))
    (format #f "~4f hr" (/ x 3600)))
   (else
    (format #f "~4f days" (/ x (* 24 3600))))))

(define (gdn-get-locale-information)
  (vector
   (vector "LC_ALL" (setlocale LC_ALL))
   (vector "LC_COLLATE" (setlocale LC_COLLATE))
   (vector "LC_CTYPE" (setlocale LC_CTYPE))
   (vector "LC_MESSAGES" (setlocale LC_MESSAGES))
   (vector "LC_MONETARY" (setlocale LC_MONETARY))
   (vector "LC_NUMERIC" (setlocale LC_NUMERIC))
   (vector "LC_TIME" (setlocale LC_TIME))))

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
   (vector "Processor Count" (number->string (or (false-if-exception (total-processor-count)) 0)))
   (vector "Processors Used" (number->string (or (false-if-exception (current-processor-count)) 0)))
   ))

(define (gdn-get-environment-variables)
  (let ((EV (sort (environ) string-ci<?)))
    (list->vector
     (map (lambda (entry)
            (let ((split-location (string-index entry #\=)))
              (vector (string-take entry split-location)
                      (string-drop entry (1+ split-location)))))
          EV))))

(define (gdn-get-time-entries)
  (let* ((timeofday (gettimeofday))
         (timeofday-sec (car timeofday))
         (timeofday-usec (cdr timeofday))
         (curtime (current-time))
         (now-local (localtime curtime))
         (now-gmt (gmtime curtime))
         (now-cpu (times)))
    (vector
     (vector "Seconds in Epoch" (__number->locale-string (+ timeofday-sec (/ timeofday-usec 1000000))))
     (vector "GMT" (strftime "%c" now-gmt))
     (vector "GMT Day of Year" (__number->locale-string (tm:yday now-gmt)))
     (vector "Local Time Zone" (tm:zone now-local))
     (vector "Time Zone Seconds from GMT" (__number->locale-string (tm:gmtoff now-local)))
     (vector "Local Time" (strftime "%c" now-local))
     (vector "Local Day of Year" (__number->locale-string (tm:yday now-local)))
     (vector "Daylight Savings" (cond ((= 0 (tm:isdst now-local)) "no")
                                      ((< 0 (tm:isdst now-local)) "yes")
                                      ((> 0 (tm:isdst now-local)) "unknown")))
     (vector "CPU Clock Time" (gdn-time-string (/ (tms:clock now-cpu)
                                              (exact->inexact internal-time-units-per-second))))
     (vector "CPU Process Time" (gdn-time-string (/ (tms:utime now-cpu)
                                                (exact->inexact internal-time-units-per-second))))
     (vector "CPU System Time" (gdn-time-string (/ (tms:stime now-cpu)
                                               (exact->inexact internal-time-units-per-second))))
     (vector "CPU CU Time" (gdn-time-string (tms:cutime now-cpu)))
     (vector "CPU CS Time" (gdn-time-string (tms:cstime now-cpu)))
     (vector "Guile Time Units Per Sec" (__number->locale-string internal-time-units-per-second))
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

(define (gdn-get-gc-stats)
  (let ((x (gc-stats)))
    (vector 
     (vector "GC Time Taken" (gdn-time-string (/ (assoc-ref x 'gc-time-taken) (exact->inexact internal-time-units-per-second))))
     (vector "Heap Size" (gdn-memory-string (assoc-ref x 'heap-size)))
     (vector "Heap Free Size" (gdn-memory-string (assoc-ref x 'heap-free-size)))
     (vector "Heap Total Allocated" (gdn-memory-string (assoc-ref x 'heap-total-allocated)))
     (vector "Heap Allocated Since GC" (gdn-memory-string (assoc-ref x 'heap-allocated-since-gc)))
     (vector "Protected Objects" (__number->locale-string (assoc-ref x 'protected-objects)))
     (vector "GC Events" (__number->locale-string (assoc-ref x 'gc-times)))
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
   (vector "environment" (gdn-get-environment-variables))
   (vector "locale" (gdn-get-locale-information))
   (vector "process" (gdn-get-process-information))
   (vector "time" (gdn-get-time-entries))
   (vector "user" (gdn-get-user-information-entries))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point for REPL

;; The REPL entry point is different than the standard REPL
;; in that
;; - it uses the Guidance 'run' error handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point for running

;; 'Run' error handler

;; When running code that creates an error, we want to
;; end up in a debug mode.
;; Capture the stack for the error.
;; Update the backtrace and environment info.
;; Enter a debug mode where
;; -  the limited debug repl works.
;  -  allow traps to be created or destroyed
;; -  allow "stop", "restart"
;; If "stop" or "restart", the stack is unwound.


;; (call-with-error-handling repl-handler
;;   #:on-error error-handler
;;   #:trap-handler trap-handler)

;; (define (repl-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break function handling

;; Break function handling is similar to run error handler. It
;; is caused by placing a call to (*break*) in the code, or by
;; clicking the pause button.

;; It has the same features as the 'run' error handler plus
;; it allows all the step and next actions, as well as the
;; continue action.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trap error handling

;; Basically the same as the Break handler except
;; for its entry point.  Also, it highlights
;; the trap that triggered it in the trap list view until
;; the source location changes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting traps

;; You can set traps by
;; - using the add-trap commands at the REPL
;; - right-clicking on a source file
;; - choosing a function from the module explorer

;; (interaction-environment) is the same as
;; (current-module) for the current thread.

(define (gdn-run-repl)
  (let loop ()
    (unless (char-ready?)
      (display "poop>" %gdn-prompt-port))
    (force-output)
    (run-hook before-read-hook)
    (%gdn-update-thread-info)
    (%gdn-update-environment-info (gdn-get-environment))
    (%gdn-update-frame-info
     (gdn-get-backtrace
      (vector-ref (__stack->vector (make-stack #t
                                               8 ; layers to get out of repl-reader
                                               )) 0)))
    (let ((input (read-line (current-input-port))))
      (let ((result (false-if-exception (eval-string input))))
        (write result)
        (loop)))))

(define (gdn-run-argv argv)
  "Process the command line argument list and then run a Guile
interpreter."
  ;;(eval (__compile-shell-switches argv) (current-module)))
  (display "ARGV: ")
  (write argv)
  (newline)
  (call-with-error-handling (__compile-shell-switches argv)))

(define gdn-run-trap (__add-trap-at-procedure-call! gdn-run-argv))

(define (gdn-run-trap-enable)
  (false-if-exception (__enable-trap! gdn-run-trap)))

(define (gdn-run-trap-disable)
  (false-if-exception (__disable-trap! gdn-run-trap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; Note that the GC and Sweep handlers are created with the C API

(define (gdn-load-hook filename)
  "Inform Guidance every time a file is loaded"
  (%gdn-load-handler filename))

(set! %load-hook gdn-load-hook)

(define (gdn-module-defined-hook directory)
  "Inform Guidance every time a module is loaded"
  (let* ((name (module-name directory))
         ;; NAME is a list of symbols, e.g. '(srfi srfi-1)
         (name-str (format #f "~a" name))
         (filename ((record-accessor module-type 'filename) directory))
         (abs-filename (%search-load-path filename))
         (category
          (cond
           ((gdn-string-starts-with (%library-dir) abs-filename)
            'library)
           ((gdn-string-starts-with (%site-dir) abs-filename)
            'site)
           ((gdn-string-starts-with (%global-site-dir) abs-filename)
            'global-site)
           ((gdn-string-starts-with (%package-data-dir) abs-filename)
            'package-data)
           (else
            'other))))
    (%gdn-module-defined-handler
     (vector name-str filename abs-filename category))))

(add-hook! module-defined-hook gdn-module-defined-hook)

(define (gdn-exit-hook)
  "Inform Guidance that this repl is about to quit"
  (%gdn-exit-handler))

(add-hook! exit-hook gdn-exit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader

;; Like a regular REPL reader, except that the prompt is printed to
;; %gdn-prompt-port. Does not handle readline-like commands.  Each
;; iteration, it runs the help functions that update the information
;; GtkStack pages.



(define gdn-repl-reader
  (lambda* (prompt #:optional (reader (fluid-ref current-reader)))
    (unless (char-ready?)
      (display (if (string? prompt) prompt (prompt)) %gdn-prompt-port))
    (force-output)
    (run-hook before-read-hook)
    (%gdn-update-thread-info)
    (%gdn-update-environment-info (gdn-get-environment))
    ;;  (%gdn-update-trap-info trap-idx)
    (%gdn-update-frame-info
     (gdn-get-backtrace
      (vector-ref (__stack->vector (make-stack #t
                                               8 ; layers to get out of repl-reader
                                               )) 0)))
    ((or reader read) (current-input-port))))

(set! repl-reader gdn-repl-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trap Handler

;; The trap handler receives
;; - a <frame>
;; - trap index
;; - trap name
;; see debug-trap-handler in (system repl error-handling)
;;

;; - extracts all of the current stack, locals, threads, env into the
;;   tree models
;; - walks up the tree to find a source location, and, if found, highlights
;;   that in the appropriate file viewer
;; - signals that it is paused
;; - prints the "debug>" prompt
;; - waits until it either receives a continue / step / return / stop or a 'eval' + entry string
;;   - if it is an evalentry string, just evaluate it under (false-if-exception)
;;   - if it is a step, adds the appropriate emphemeral trap, if necessary
;; - returns to continue execution

(define (gdn-trap-handler frame trap-idx trap-name)
  "A trap and break handler. To indicate a break, TRAP-IDX should be
#f and TRAP-NAME should be 'break'.  Otherwise the trap-idx and
trap-name indicate the current trap."
  (%gdn-set-trap-buttons)
  (%gdn-update-thread-info)
  (%gdn-update-environment-info (gdn-get-environment))
  (%gdn-update-trap-info trap-idx)
  (%gdn-update-frame-info (gdn-get-backtrace frame))
  (display "trap>" %gdn-prompt-port)
  
  ;; This is a blocking operation, awaiting a response from the
  ;; operator. This cannot be run in the Gtk main thread.
  (let loop ((response (%gdn-get-trap-response)))
    (let ((type (car response))
          (data (cdr response)))
      (cond
       ((eq? type 'step-into-instruction)
        (add-ephemeral-stepping-trap! frame gdn-trap-handler #:into? #t #:instruction? #t))
       ((eq? type 'step-into)
        (add-ephemeral-stepping-trap! frame gdn-trap-handler #:into? #t #:instruction? #f))
       ((eq? type 'step-instruction)
        (add-ephemeral-stepping-trap! frame gdn-trap-handler #:into? #f #:instruction? #t))
       ((eq? type 'step)
        (add-ephemeral-stepping-trap! frame gdn-trap-handler #:into? #f #:instruction? #t))
       ((eq? type 'step-out)
        (add-ephemeral-trap-at-frame-finish! frame gdn-trap-handler))
       ((eq? type 'continue)
        (%gdn-disable-trap-buttons)
        *unspecified*)
       ((eq? type 'restart)
        (%gdn-disable-trap-buttons)
        (abort-to-prompt *start-prompt* "trap restart"))
       ((eq? type 'eval)
        (display data (current-output-port))
        (newline (current-output-port))
        (display "=> " (current-output-port))
        (write (false-if-exception (eval-string data)) (current-output-port))
        (newline (current-output-port))
        (loop (%gdn-get-trap-response)))))))

(define (gdn-error-handler frame)
  "An error handler"
  (%gdn-enable-error-buttons)
  (%gdn-update-thread-info)
  (%gdn-update-environment-info (gdn-get-environment))
  (%gdn-update-trap-info #f)
  (%gdn-update-frame-info (gdn-get-backtrace frame))
  (display "error>" %gdn-prompt-port)
  
  ;; This is a blocking operation, awaiting a response from the
  ;; operator. This cannot be run in the Gtk main thread.
  (let loop ((response (%gdn-get-error-response)))
    (let ((type (car response))
          (data (cdr response)))
      (cond
       ((eq? type 'continue)
        *unspecified*)
       ((eq? type 'restart)
        (%gdn-disable-error-buttons)
        (abort-to-prompt *start-prompt* "error restart"))
       ((eq? type 'eval)
        (display data (current-output-port))
        (newline (current-output-port))
        (display "=> " (current-output-port))
        (write (false-if-exception (eval-string data)) (current-output-port))
        (newline (current-output-port))
        (loop (%gdn-get-error-response)))))))

