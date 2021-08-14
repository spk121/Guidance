;;; This code is sent to the main Guile thread before the
;;; REPL is launched

(use-modules
 (ice-9 command-line)
 (ice-9 top-repl)
 (srfi srfi-1)
 (system vm trap-state)
 (system vm vm)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; Note that the GC and Sweep handlers are created with the C API

(define (gdn-load-hook filename)
  "Inform Guidance every time a file is loaded"
  (gdn-load-handler filename gdn-self))

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
           ((gdn-string-starts-with (%package-data-dir) abs_filename)
            'package-data)
           (else
            'other))))
    (gdn-module-defined-handler
     (list name-str filename abs-filename category)
     gdn-self)))

(add-hook! module-defined-hook gdn-module-defined-hook)

(define (gdn-exit-hook)
  "Inform Guidance that this repl is about to quit"
  (gdn-exit-handler gdn-self))

(add-hook! exit-hook gdn-exit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader

;; Like a regular REPL reader, except that the prompt is printed to
;; gdn-prompt-port. Does not handle readline-like commands
(define gdn-repl-reader
  (lambda* (prompt #:optional (reader (fluid-ref current-reader)))
    (unless (char-ready?)
      (display (if (string? prompt) prompt (prompt))
               gdn-prompt-port))
    (force-output)
    (run-hook before-read-hook)
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

(define* (gdn-trap-and-break-handler frame #:optional (trap-idx #f) (trap-name "break"))
  "A trap and break handler. To indicate a break, TRAP-IDX should be
#f and TRAP-NAME should be 'break'.  Otherwise the trap-idx and
trap-name indicate the current trap."
  (gdn-update-thread-info gdn-self)
  (gdn-update-environment-info gdn-self (gdn-get-environment))
  (gdn-update-trap-info gdn-self trap-idx)
  (gdn-update-backtrace-info (gdn-get-backtrace frame))

  (display (current-prompt-port) "debug>")
  
  ;; This is a blocking operation, awaiting a response from the
  ;; operator. This cannot be run in the Gtk main thread.
  (let* ((response (gdn-get-trap-response))
         (type (car response))
         (data (cdr response)))
    (cond
     ((eq? type 'step-into-instruction)
      #f)
     ((eq? type 'step-into)
      #f)
     ((eq? type 'step-instruction)
      #f)
     ((eq? type 'step)
      #f)
     ((eq? type 'step-out)
      #f)
     ((eq? type 'continue)
      #f)
     ((eq? type 'restart)
      #f)
     ((eq? type 'eval)
      (display data (current-output-port))
      (newline (current-output-port))
      (display "=> " (current-output-port))
      (write (false-if-exception (eval-string data)) (current-output-port))
      (newline (current-output-port))
      (loop)
      #f))))

(define (gdn-break)
  (gdn-trap-and-break-handler))
;;
;; I suppose we hook the step and return buttons to
;;  add-ephemeral-trap-at-frame-finish! and
;;  add-ephemeral-stepping-trap!
;;

;; There are functions to list the traps
;; (list-traps)
;; (enable-trap! idx)
;; (disable-trap! idx)
;; (delete-trap! idx)


(install-trap-handler! gdn-trap-and-break-handler)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break Handler

;; The break handler is thunk run as an async in the default thread.
;; It just computes the frame just above this thunk and then runs the
;; trap handler.



#|
  (let ((entry-point (compile-shell-switches argv)
       (restore-signals)
       (usleep 10)
       (eval entry-point (current-module))
       (usleep 10)
       0))


(define yjd-invalid-command-line-arguments
  '("--no-debug"
    "--debug"))

(define (yjd-rejigger-program-arguments argv)
  "Give a list of command line arguments, this elides
arguments that can't be handled by YJDebug."
  (let* ((argv2
          (fold
           (lambda (x prev)
             (if (member x yjd-invalid-command-line-arguments)
                 prev
                 (append prev (list x))))
           '()
           argv)))
    argv2))

(define (yjd-make-entry-point argv)
  "This parses the command line arguments to create a procedure that
can be called as the entry point of this script.  This procedure lives
in its own prompt."
  ;; When the real Guile interpreter spawns, it checks to see
  ;; if it should call setlocale.  But since this interpreter
  ;; is already running in Guile, I guess we assume that it is
  ;; taken care of.
  ;; (when (should-install-locale?)
  ;;   (setlocale LC_ALL ""))

  ;; Maybe we need to cull some standard Guile command-line arguments.
  (set-program-arguments (yjd-rejigger-program-arguments argv))

  ;; Generically we'd run (ice-9 top-repl) but if we're passed a
  ;; script, we'll start there
  (compile-shell-switches argv))

(define (yjd-run argv)
  "Process the command line arguemnt list and then run a Guile
interpreter."
  (let ((entry-point (yjd-make-entry-point argv)))
       (restore-signals)
       (usleep 10)
       (eval entry-point (current-module))
       (usleep 10)
       0))

;; The trap handler receives
;; - a <frame>
;; - trap index
;; - trap name
;; see debug-trap-handler in (system repl error-handling)
;;
;; We want the new trap handler to
;; - extract the stack ahd locals into the GtkTreeModel
;; - signal that it is paused
;; - wait until it receives a continue / step / return / stop
;; - add the appropriate emphemeral trap, if necessary
;; - return to continue execution
;;
;; I suppose we hook the step and return buttons to
;;  add-ephemeral-trap-at-frame-finish! and
;;  add-ephemeral-stepping-trap!
;;

;; There are functions to list the traps
;; (list-traps)
;; (enable-trap! idx)
;; (disable-trap! idx)
;; (delete-trap! idx)


(install-trap-handler! yjd-trap-handler)

;; add-trap-at-procedure-call! returns in the index
;; of the trap
(add-trap-at-procedure-call! yjd-run)
(yjd-run (command-line))
|#
