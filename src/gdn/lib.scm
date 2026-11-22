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

;; (gdn lib) contains the procedures that do no recall any special
;; primitives from Guidance, e.g, they should run in base Guile.

(define-module (gdn lib)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (system vm debug)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm traps)
  #:use-module (system vm vm)
  #:export (gdn-error-frames
            gdn-frame-frames
            guile-internal-frame?
            guidance-frame?
            gdn-loaded-modules
            gdn-module-list
            gdn-module-procedures
            gdn-trap-at-line
            gdn-trap-at-binding
            gdn-procedure-binding
            gdn-loaded-code-at-line?
            code-extent
            code-line-ranges
            ip-in-ranges?
            gdn-binding-readable?
            gdn-parse-command-line
            gdn-get-backtrace
            gdn-get-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMAND LINE

;; Guidance takes a subset of Guile's command-line options, so that it
;; can be called the way Guile would be.  main.c handles --help and
;; --version itself, before starting the GUI, using the same rules
;; for where the options end, and has the usage message.

(define (gdn-parse-command-line args)
  "Parses Guidance's command-line arguments, ARGS, not including the
program name.  Returns an alist with these keys:

  load-path       directories from -L, in order
  compiled-path   directories from -C, in order
  load-files      files from -l, in order
  script          the program FILE, or #f
  expression      the program EXPR from -c, or #f
  entry           the FUNCTION from -e, as a symbol, or #f
  arguments       the program's ARGs
  errors          a list of messages about bad arguments"
  (let loop ((args args)
             (load-path '()) (compiled-path '()) (load-files '())
             (entry #f) (errors '()))
    (define (finish script expression arguments errors)
      `((load-path . ,(reverse load-path))
        (compiled-path . ,(reverse compiled-path))
        (load-files . ,(reverse load-files))
        (script . ,script)
        (expression . ,expression)
        (entry . ,entry)
        (arguments . ,arguments)
        (errors . ,(reverse errors))))
    (define (missing option)
      (finish #f #f '()
              (cons (format #f "option ~a requires an argument" option)
                    errors)))
    (if (null? args)
        (finish #f #f '() errors)
        (let ((arg (car args))
              (rest (cdr args)))
          (cond
           ((member arg '("-L" "-C" "-l" "-e" "-s" "-c"))
            (if (null? rest)
                (missing arg)
                (let ((value (car rest)))
                  (cond
                   ((string=? arg "-L")
                    (loop (cdr rest) (cons value load-path) compiled-path
                          load-files entry errors))
                   ((string=? arg "-C")
                    (loop (cdr rest) load-path (cons value compiled-path)
                          load-files entry errors))
                   ((string=? arg "-l")
                    (loop (cdr rest) load-path compiled-path
                          (cons value load-files) entry errors))
                   ((string=? arg "-e")
                    (loop (cdr rest) load-path compiled-path load-files
                          (string->symbol value) errors))
                   ((string=? arg "-s")
                    (finish value #f (cdr rest) errors))
                   (else              ; -c
                    (finish #f value (cdr rest) errors))))))
           ((string=? arg "--")
            (if (null? rest)
                (finish #f #f '() errors)
                (finish (car rest) #f (cdr rest) errors)))
           ((and (> (string-length arg) 1) (char=? (string-ref arg 0) #\-))
            (loop rest load-path compiled-path load-files entry
                  (cons (format #f "unrecognized option ~a" arg) errors)))
           (else
            (finish arg #f rest errors)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STACKS

;; Guile's own source files are recorded with paths relative to the
;; load path, e.g. "ice-9/boot-9.scm".  So are Guidance's own modules:
;; Guidance gives them these names when it compiles them from its
;; resources, and they have them when loaded from files, as in the
;; tests.
(define %guidance-source-prefix "gdn/")

(define %guile-source-prefixes
  (list "ice-9/" "system/" "srfi/" "language/" "rnrs/" "oop/" "scheme/"
        "web/" "sxml/" "texinfo/" %guidance-source-prefix))

(define (guidance-frame? frame)
  "True if FRAME is running Guidance's own code."
  (let* ((source (frame-source frame))
         (file (and source (source:file source))))
    (and (string? file)
         (string-prefix? %guidance-source-prefix file))))

(define (guile-internal-frame? frame)
  "True if FRAME is running code from Guile's own libraries, or from
Guidance's."
  (let* ((source (frame-source frame))
         (file (and source (source:file source))))
    (and (string? file)
         (any (lambda (prefix) (string-prefix? prefix file))
              %guile-source-prefixes))))

;; Backtraces show at most this many frames, innermost first, so that
;; deep recursion doesn't make them enormous.
(define %max-backtrace-frames 500)

(define (stack->frame-list stack)
  (let loop ((i (1- (min (stack-length stack) %max-backtrace-frames)))
             (frames '()))
    (if (< i 0)
        frames
        (loop (1- i) (cons (stack-ref stack i) frames)))))

(define (drop-through-last pred frames)
  "Drops FRAMES up to and including the last frame satisfying PRED.
Returns FRAMES unchanged if no frame does."
  (let loop ((rest frames) (result frames))
    (cond
     ((null? rest) result)
     ((pred (car rest)) (loop (cdr rest) (cdr rest)))
     (else (loop (cdr rest) result)))))

(define (frame-has-source-file? frame)
  (let ((source (frame-source frame)))
    (and source (string? (source:file source)))))

(define (trim-outer-internal-frames frames)
  "Drops the outermost frames that belong to Guile's libraries or have
no source file, such as the machinery of `load'.  Never drops every
frame."
  (let ((trimmed (reverse (drop-while
                           (lambda (f)
                             (or (guile-internal-frame? f)
                                 (not (frame-has-source-file? f))))
                           (reverse frames)))))
    (if (null? trimmed) frames trimmed)))

(define (gdn-error-frames stack-tag)
  "Call from within a pre-unwind exception handler.  Returns the list
of frames, innermost first, between the point where the exception was
raised and the prompt STACK-TAG, without the frames of Guile's exception
machinery or of the handler itself."
  (let* ((frames (stack->frame-list (make-stack #t 0 stack-tag)))
         ;; Everything up to the raise is the handler and its callers.
         (frames (drop-through-last
                  (lambda (f) (eq? (frame-procedure-name f) 'raise-exception))
                  frames))
         ;; Then drop helpers like `error' and `throw' so the
         ;; innermost frame is the code that raised the exception.
         (user (drop-while guile-internal-frame? frames)))
    (trim-outer-internal-frames (if (null? user) frames user))))

(define (gdn-frame-frames frame stack-tag)
  "Returns the list of frames, innermost first, from FRAME out to the
prompt STACK-TAG.  FRAME is kept even if it is in Guile's libraries,
since it may be the subject of a trap."
  (let ((frames (stack->frame-list (make-stack frame 0 stack-tag))))
    (if (null? frames)
        frames
        (let ((outer (trim-outer-internal-frames (cdr frames))))
          (cons (car frames)
                ;; If every outer frame is internal, the trim keeps
                ;; them all; drop them all instead.
                (if (every (lambda (f)
                             (or (guile-internal-frame? f)
                                 (not (frame-has-source-file? f))))
                           outer)
                    '()
                    outer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULES AND PROCEDURES

(define (absolute-source-file file)
  "Guile records the source files of its own libraries relative to the
load path.  Returns FILE as an absolute path if it can be found, or
else FILE itself."
  (cond
   ((not (string? file)) #f)
   ((absolute-file-name? file) file)
   ((%search-load-path file) => identity)
   (else file)))

(define (gdn-loaded-modules)
  "Returns the loaded modules, except Guidance's own, sorted by name."
  ;; Guile has no documented way to list the loaded modules.  Walking
  ;; the module tree from the root with `module-submodules' is how
  ;; Guile's own (system xref) finds them.
  (let ((seen (make-hash-table)))
    (let loop ((pending (list (resolve-module '() #f)))
               (modules '()))
      (if (null? pending)
          (sort modules
                (lambda (a b)
                  (string<? (object->string (module-name a))
                            (object->string (module-name b)))))
          (let* ((m (car pending))
                 (subs (filter (lambda (sub)
                                 (and (not (hashq-ref seen sub))
                                      (hashq-set! seen sub #t)))
                               (hash-map->list (lambda (k v) v)
                                               (module-submodules m)))))
            (loop (append subs (cdr pending))
                  (if (and (eq? (module-kind m) 'directory)
                           (pair? (module-name m))
                           (not (eq? (car (module-name m)) 'gdn)))
                      (cons m modules)
                      modules)))))))

(define (gdn-module-list)
  "Returns a vector with an entry #(NAME FILE) for each loaded module.
NAME is the module name as a string, like \"(ice-9 format)\".  FILE is
the absolute path of its source, or \"\" if it is not known."
  (list->vector
   (map (lambda (m)
          (vector (object->string (module-name m))
                  (or (absolute-source-file (module-filename m)) "")))
        (gdn-loaded-modules))))

(define (procedure-location proc)
  "Returns two values: the absolute source file of PROC and its 1-based
line, or #f and 0 if they are unknown."
  (let ((sources (if (program? proc) (program-sources proc) '())))
    (if (pair? sources)
        (let ((source (car sources)))     ; (ip file line . column)
          (values (absolute-source-file (cadr source))
                  (1+ (caddr source))))
        (values #f 0))))

(define (gdn-module-procedures module-name-string)
  "Returns a vector with an entry #(NAME FILE LINE TRAPPABLE?) for each
procedure defined in the module named by MODULE-NAME-STRING, sorted by
name.  FILE is an absolute path, or \"\" if it is unknown, in which case
LINE is 0.  Only compiled procedures are TRAPPABLE?."
  (let* ((name (false-if-exception
                (call-with-input-string module-name-string read)))
         (module (and (list? name)
                      (resolve-module name #f #f #:ensure #f)))
         (entries '()))
    (when module
      (module-for-each
       (lambda (sym var)
         (when (variable-bound? var)
           (let ((value (variable-ref var)))
             (when (procedure? value)
               (call-with-values (lambda () (procedure-location value))
                 (lambda (file line)
                   (set! entries
                     (cons (vector (symbol->string sym)
                                   (or file "")
                                   line
                                   (program? value))
                           entries))))))))
       module))
    (list->vector
     (sort entries (lambda (a b) (string<? (vector-ref a 0)
                                           (vector-ref b 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BREAKPOINTS AT SOURCE LINES

;; Guile's add-trap-at-source-location! finds procedures through
;; (system xref), whose index is not updated for top-level definitions
;; made by `load', and it only covers code loaded when the trap is
;; made.  Guidance's line traps instead attach themselves to each
;; procedure with code at the line: to those already loaded when the
;; trap is made, and to any other when it is first called.  So a
;; breakpoint can be set before its code is loaded, and survives the
;; program being loaded again.

(define (same-source-file? recorded file)
  (and (string? recorded)
       (or (string=? recorded file)
           (equal? (absolute-source-file recorded) file))))

(define (code-extent code)
  "Returns the start and end addresses of the procedure whose code
includes the address CODE, or #f and #f."
  (let ((info (find-program-debug-info code)))
    (if info
        (values (program-debug-info-addr info)
                (+ (program-debug-info-addr info)
                   (program-debug-info-size info)))
        (values #f #f))))

;; Decoding a procedure's line table from its debug information is
;; slow, and finding the code at a line looks at every loaded
;; procedure.  A procedure's code never changes once loaded, so each
;; table is decoded once and kept, keyed by the code's start address.
;; Only the REPL thread, which runs the traps, uses it.
(define *line-tables* (make-hash-table))

(define (code-line-table start end)
  "Returns the line table of the procedure whose code runs from START
to END: a list of entries (FILE LINE PC . NEXT-PC), one for each
source position."
  (or (hashv-ref *line-tables* start)
      (let ((table
             (let loop ((sources (find-program-sources start)) (acc '()))
               (if (null? sources)
                   (reverse! acc)
                   (let* ((source (car sources))
                          (next (if (pair? (cdr sources))
                                    (source-post-pc (cadr sources))
                                    end)))
                     (loop (cdr sources)
                           (cons (cons* (source-file source)
                                        (source-line source)
                                        (source-post-pc source)
                                        next)
                                 acc)))))))
        (hashv-set! *line-tables* start table)
        table)))

(define (code-line-ranges code file line)
  "Returns the instruction ranges, as a list of absolute (START . END)
pairs, of the code at LINE of FILE in the procedure whose code begins
at CODE.  LINE is 0-based.  Returns '() if the procedure has no code
at LINE.  (Lines with no code are not moved to the next line with
code: procedures nest, so the next line may belong to another.)"
  (call-with-values (lambda () (code-extent code))
    (lambda (start end)
      (if start
          (filter-map (lambda (entry)
                        (and (eqv? (cadr entry) line)
                             (same-source-file? (car entry) file)
                             (cddr entry)))
                      (code-line-table start end))
          '()))))

(define (procedures-at-line file line)
  "Returns a list of (CODE . RANGES) for the compiled procedures in the
loaded modules that have code at LINE (0-based) of FILE, where CODE is
the address of the procedure's code."
  (let ((seen (make-hash-table)))
    (append-map
     (lambda (module)
       (let ((found '()))
         (module-for-each
          (lambda (sym var)
            (when (variable-bound? var)
              (let ((value (variable-ref var)))
                (when (and (program? value)
                           (not (hashv-ref seen (program-code value))))
                  (let* ((code (program-code value))
                         (ranges (code-line-ranges code file line)))
                    (hashv-set! seen code #t)
                    (when (pair? ranges)
                      (set! found (cons (cons code ranges) found))))))))
          module)
         found))
     (gdn-loaded-modules))))

(define (gdn-loaded-code-at-line? file user-line)
  "True if a loaded procedure has code at USER-LINE (1-based) of FILE."
  (pair? (procedures-at-line file (1- user-line))))

(define (ip-in-ranges? ip ranges)
  (any (lambda (range) (and (<= (car range) ip) (< ip (cdr range))))
       ranges))

(define (code-range-trap code ranges handler current-frame)
  "An enabled low-level trap calling HANDLER when execution enters
RANGES in the procedure whose code begins at CODE."
  (call-with-values (lambda () (code-extent code))
    (lambda (start end)
      (trap-at-procedure-ip-in-range
       ;; The procedure itself is only needed to recognize its frames,
       ;; which OUR-FRAME? does from the code's addresses instead.
       identity ranges handler
       #:current-frame current-frame
       #:our-frame? (lambda (frame)
                      (let ((ip (frame-instruction-pointer frame)))
                        (and (<= start ip) (< ip end))))))))

(define* (gdn-trap-at-line file user-line handler #:optional (current-frames '()))
  "Creates an enabled low-level trap that calls HANDLER with the frame
whenever execution reaches USER-LINE (1-based) of FILE, an absolute
path.

CURRENT-FRAMES are the frames of a paused program.  Those already at
the line will return into it when the program resumes; that is not
reaching the line, so their first hit is ignored."
  (define line (1- user-line))
  (define (enable current-frames)
    (let ((subtraps '())
          (covered (make-hash-table))    ; code address -> #t
          (seen (make-hash-table))       ; entry address -> #t
          (already-there #f))
      (define (line-handler frame)
        (let ((address (frame-address frame)))
          (if (memv address already-there)
              (set! already-there (delv address already-there))
              (handler frame))))
      (define (attach! code ranges current-frame)
        (hashv-set! covered code #t)
        (set! subtraps
          (cons (code-range-trap code ranges line-handler current-frame)
                subtraps)))
      ;; Attach to procedures as they are called.
      (define (apply-hook frame)
        (let ((ip (frame-instruction-pointer frame)))
          (unless (hashv-ref seen ip)
            (hashv-set! seen ip #t)
            (call-with-values (lambda () (code-extent ip))
              (lambda (start end)
                (when (and start (not (hashv-ref covered start)))
                  (let ((ranges (code-line-ranges start file line)))
                    (if (pair? ranges)
                        (attach! start ranges frame)
                        (hashv-set! covered start #t)))))))))
      ;; Attach to the procedures already loaded.
      (let ((loaded (procedures-at-line file line)))
        (set! already-there
          (filter-map (lambda (frame)
                        (and (ip-in-ranges? (frame-instruction-pointer frame)
                                            (append-map cdr loaded))
                             (frame-address frame)))
                      current-frames))
        (for-each (lambda (match) (attach! (car match) (cdr match) #f))
                  loaded))
      (vm-add-apply-hook! apply-hook)
      ;; The trap interface: calling it disables the trap and returns
      ;; a procedure that enables it again.
      (lambda* (#:optional frame)
        ;; Guile's procedure traps need a frame when disabled inside
        ;; their procedure, which is the case if the program was
        ;; stopped there; trap-state doesn't pass one.
        (let ((frame (or frame (stack-ref (make-stack #t) 0))))
          (vm-remove-apply-hook! apply-hook)
          (for-each (lambda (trap) (trap frame)) subtraps))
        (lambda* (#:optional frame)
          (enable '())))))
  (enable current-frames))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BREAKPOINTS AT PROCEDURES

;; Guile's trap-at-procedure-call traps one procedure object.  Running
;; a program again defines its procedures again, as new objects, so
;; such a trap would silently stop firing.  Guidance's procedure traps
;; instead belong to a binding, a variable of a module, and trap calls
;; to whichever procedure it holds at the time.

(define (gdn-procedure-binding proc module)
  "Returns the variable that holds the procedure PROC and its name, a
symbol, as two values, or #f and #f if PROC is not the value of a
binding with its own name.  MODULE is looked in first, then the other
loaded modules."
  (let ((name (procedure-name proc)))
    (define (binding-in module)
      (let ((variable (module-variable module name)))
        (and variable
             (variable-bound? variable)
             (eq? (variable-ref variable) proc)
             variable)))
    (let ((variable (and (symbol? name)
                         (or (binding-in module)
                             (any binding-in (gdn-loaded-modules))))))
      (if variable
          (values variable name)
          (values #f #f)))))

(define (gdn-trap-at-binding variable handler)
  "Creates an enabled low-level trap that calls HANDLER with the frame
whenever the procedure that VARIABLE holds at the time is called.  As
with Guile's own procedure traps, procedures that share their code,
such as the closures made by one `lambda', are not told apart."
  (define (apply-hook frame)
    (when (variable-bound? variable)
      (let ((proc (variable-ref variable)))
        (when (and (program? proc)
                   (= (frame-instruction-pointer frame) (program-code proc)))
          (handler frame)))))
  ;; The trap interface: calling an enabled trap disables it and
  ;; returns a procedure that enables it again, and so on.
  (define (enable)
    (vm-add-apply-hook! apply-hook)
    (lambda* (#:optional frame)
      (vm-remove-apply-hook! apply-hook)
      (lambda* (#:optional frame)
        (enable))))
  (enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKTRACE

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
(define (call-argument-count args)
  "The number of arguments in ARGS, the arguments of a call
representation.  When a rest argument is not available, Guile gives
them as an improper list, ending in _ rather than ()."
  (let loop ((args args) (n 0))
    (if (pair? args) (loop (cdr args) (1+ n)) n)))

(define (frame->backtrace-entry frame top-frame?)
  "A backtrace entry for FRAME.  This must not fail: it is called while
the program is paused in a VM hook, and an exception that unwinds
through a hook can corrupt the program.  So if anything goes wrong,
the entry just says less."
  (catch #t
    (lambda () (frame->backtrace-entry* frame top-frame?))
    (lambda args
      (vector (format #f "(~a)" (or (false-if-exception
                                     (frame-procedure-name frame))
                                    "λ"))
              "" 0 0 0 (vector)))))

(define (frame->backtrace-entry* frame top-frame?)
  ;; A frame may be inspected at any instruction, where some of its
  ;; information may not be available, so each part falls back to a
  ;; default rather than failing.  TOP-FRAME? tells Guile that FRAME
  ;; is the innermost frame, so arguments not yet copied into local
  ;; variables can still be read, e.g. at a procedure-call trap.
  (let* ((source (false-if-exception (frame-source frame)))
         (file (if source (or (source:file source) "") ""))
         (line (if source (source:line-for-user source) 0))
         (col (if source (source:column source) 0))
         (call (false-if-exception
                (frame-call-representation frame #:top-frame? top-frame?)))
         (nargs (if (pair? call) (call-argument-count (cdr call)) 0))
         (bindings (or (false-if-exception
                        (frame-bindings frame top-frame?))
                       '())))
    (vector
     (or (and (pair? call) (format-call call))
         (format #f "(~a)" (or (false-if-exception
                                (frame-procedure-name frame))
                               "λ")))
     file
     line
     col
     nargs
     (list->vector
      (filter-map (lambda (binding)
                    ;; Unnamed bindings are compiler temporaries.
                    (and (binding-name binding)
                         (gdn-binding-readable? frame binding)
                         (false-if-exception (unpack-binding nargs binding))))
                  bindings)))))

(define* (gdn-get-backtrace frames #:optional top-frame?)
  "FRAMES is a list of frames, innermost first, as returned by
gdn-error-frames or gdn-frame-frames, or #f for an empty backtrace.
TOP-FRAME? should be true if the first frame is the one executing, as
when the program is paused by a trap.  Returns a vector of backtrace
entries, as described above."
  (if (and frames (pair? frames))
      (let ((entries
             (list->vector
              (cons (frame->backtrace-entry (car frames) top-frame?)
                    (map (lambda (f) (frame->backtrace-entry f #f))
                         (cdr frames))))))
        (name-top-level! entries)
        entries)
      (vector)))

(define (name-top-level! entries)
  "The outermost frame of a program, if it is a procedure with no name
and no arguments, is the top level of a file, or an expression from the
REPL: show it as such, rather than as (λ)."
  (let* ((last (1- (vector-length entries)))
         (entry (vector-ref entries last)))
    (when (and (string=? (vector-ref entry 0) "(λ)")
               (zero? (vector-ref entry 4)))
      (vector-set! entry 0 "(top level)"))))

(define (object->extra-info x)
  (cond
   ;; ((procedure? x)
   ;;  (or (procedure-name x) "λ"))
   ;; ((and (variable? x) (variable-bound? x))
   ;;  (value->string (variable-ref x)))
   ;; ((and (variable? x) (not (variable-bound? x)))
   ;;  "undefined")
   ((fluid? x)
    (object->string (or (false-if-exception (fluid-ref x)) "unbound")))
   (else
    "")))

(define (format-call call)
  "Formats a call representation (PROCEDURE-NAME ARG ...), with each
argument on its own line.  The arguments may be an improper list,
ending in _ for a rest argument that is not available."
  (with-output-to-string
    (lambda ()
      (display "(")
      ;; Guile gives _ for a procedure with no name.
      (display (if (memq (car call) '(#f _)) 'λ (car call)))
      (let loop ((args (cdr call)))
        (cond
         ((pair? args)
          (newline)
          (display "  ")
          (display (car args))
          (loop (cdr args)))
         ((null? args) #t)
         (else
          (newline)
          (display "  . ")
          (display args))))
      (display ")"))))

;; The number of local slots in a frame.  Guile does not export this,
;; but its own frame-call-representation uses it for the same check as
;; gdn-binding-readable?, and there is no other way to make it.
(define frame-num-locals (@@ (system vm frame) frame-num-locals))

(define (gdn-binding-readable? frame binding)
  "True if BINDING's slot is within FRAME, so that its value can be
read.  A frame can have bindings beyond its locals, as Guile notes in
frame-call-representation, especially at an instruction in the middle
of a procedure; reading one of those reads past the frame, and can
crash Guile when the value is printed."
  (< (binding-slot binding) (frame-num-locals frame)))

(define (unpack-binding n_args binding)
  (let ((name (binding-name binding))
        (val (binding-ref binding)))
    (vector
     (if name
         (symbol->string name)
         "(anonymous)")
     (if (< (binding-index binding) n_args)
         #t
         #f)
     (object->string (binding-representation binding))
     (object->string val)
     (object->extra-info val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT INFO
(define (memory-string x)
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

(define (time-string x)
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

(define (get-locale-information)
  (vector
   (vector "LC_ALL" (setlocale LC_ALL))
   (vector "LC_COLLATE" (setlocale LC_COLLATE))
   (vector "LC_CTYPE" (setlocale LC_CTYPE))
   (vector "LC_MESSAGES" (setlocale LC_MESSAGES))
   (vector "LC_MONETARY" (setlocale LC_MONETARY))
   (vector "LC_NUMERIC" (setlocale LC_NUMERIC))
   (vector "LC_TIME" (setlocale LC_TIME))))

(define (get-system-information)
  (let ((un (uname)))
    (vector
     (vector "OS Name" (utsname:sysname un))
     (vector "Node Name" (utsname:nodename un))
     (vector "Release" (utsname:release un))
     (vector "Version" (utsname:version un))
     (vector "Machine" (utsname:machine un))
     (vector "Host Name" (gethostname))
     )))

;; Lookups that can fail, e.g. in a container with no login session
;; or when the UID has no passwd entry.  They return #f on failure.
(define (safe-passwd id) (false-if-exception (getpw id)))
(define (safe-group id) (false-if-exception (getgr id)))
(define (passwd-field accessor pw) (if pw (accessor pw) #f))
(define (group-field accessor gr) (if gr (accessor gr) #f))

(define (get-process-information)
  (vector
   (vector "CWD" (getcwd))
   (vector "file creation umask" (format #f "~5,'0o" (umask)))
   (vector "PID" (number->string (getpid)))
   (vector "Groups" (format #f "~S" (getgroups)))
   (vector "PPID" (number->string (getppid)))
   (vector "UID" (number->string (getuid)))
   (vector "UID Name" (passwd-field passwd:name (safe-passwd (getuid))))
   (vector "GID" (number->string (getgid)))
   (vector "GID Name" (group-field group:name (safe-group (getgid))))
   (vector "EUID" (number->string (geteuid)))
   (vector "EUID Name" (passwd-field passwd:name (safe-passwd (geteuid))))
   (vector "EGID" (number->string (getegid)))
   (vector "EGID Name" (group-field group:name (safe-group (getegid))))
   (vector "PGRP" (number->string (getpgrp)))
   (vector "Processor Count" (number->string (or (false-if-exception (total-processor-count)) 0)))
   (vector "Processors Used" (number->string (or (false-if-exception (current-processor-count)) 0)))
   ))

(define (get-environment-variables)
  (let ((EV (sort (environ) string-ci<?)))
    (list->vector
     (map (lambda (entry)
            (let ((split-location (string-index entry #\=)))
              (vector (string-take entry split-location)
                      (string-drop entry (1+ split-location)))))
          EV))))

(define (get-time-entries)
  (let* ((timeofday (gettimeofday))
         (timeofday-sec (car timeofday))
         (timeofday-usec (cdr timeofday))
         (curtime (current-time))
         (now-local (localtime curtime))
         (now-gmt (gmtime curtime))
         (now-cpu (times)))
    (vector
     (vector "Seconds in Epoch" (number->locale-string (+ timeofday-sec (/ timeofday-usec 1000000))))
     (vector "GMT" (strftime "%c" now-gmt))
     (vector "GMT Day of Year" (number->locale-string (tm:yday now-gmt)))
     (vector "Local Time Zone" (tm:zone now-local))
     (vector "Time Zone Seconds from GMT" (number->locale-string (tm:gmtoff now-local)))
     (vector "Local Time" (strftime "%c" now-local))
     (vector "Local Day of Year" (number->locale-string (tm:yday now-local)))
     (vector "Daylight Savings" (cond ((= 0 (tm:isdst now-local)) "no")
                                      ((< 0 (tm:isdst now-local)) "yes")
                                      ((> 0 (tm:isdst now-local)) "unknown")))
     (vector "CPU Clock Time" (time-string (/ (tms:clock now-cpu)
                                                  (exact->inexact internal-time-units-per-second))))
     (vector "CPU Process Time" (time-string (/ (tms:utime now-cpu)
                                                    (exact->inexact internal-time-units-per-second))))
     (vector "CPU System Time" (time-string (/ (tms:stime now-cpu)
                                                   (exact->inexact internal-time-units-per-second))))
     (vector "CPU CU Time" (time-string (tms:cutime now-cpu)))
     (vector "CPU CS Time" (time-string (tms:cstime now-cpu)))
     (vector "Guile Time Units Per Sec" (number->locale-string internal-time-units-per-second))
     (vector "Guile Real Time" (time-string (/ (get-internal-real-time) (exact->inexact internal-time-units-per-second))))
     (vector "Guile Run Time" (time-string (/ (get-internal-run-time)
                                                  (exact->inexact internal-time-units-per-second)))))))

(define (get-user-information-entries)
  (let ((uid-pw (safe-passwd (getuid)))
        (euid-pw (safe-passwd (geteuid)))
        (gid-gr (safe-group (getgid)))
        (egid-gr (safe-group (getegid))))
    (define (pw-num accessor pw)
      (and pw (number->string (accessor pw))))
    (define (gr-num accessor gr)
      (and gr (number->string (accessor gr))))
    (vector
     (vector "Login Name" (false-if-exception (getlogin)))
     (vector "User Name" (passwd-field passwd:name uid-pw))
     (vector "User ID" (number->string (getuid)))
     (vector "User Group ID" (pw-num passwd:gid uid-pw))
     (vector "User Full Name" (passwd-field passwd:gecos uid-pw))
     (vector "User Shell" (passwd-field passwd:shell uid-pw))
     (vector "User Home" (passwd-field passwd:dir uid-pw))
     (vector "Group Name" (group-field group:name gid-gr))
     (vector "Group ID" (number->string (getgid)))
     (vector "Effective User Name" (passwd-field passwd:name euid-pw))
     (vector "Effective User ID" (number->string (geteuid)))
     (vector "Effective User Group ID" (pw-num passwd:gid euid-pw))
     (vector "Effective User Full Name" (passwd-field passwd:gecos euid-pw))
     (vector "Effective User Shell" (passwd-field passwd:shell euid-pw))
     (vector "Effective User Home" (passwd-field passwd:dir euid-pw))
     (vector "Effective Group Name" (group-field group:name egid-gr))
     (vector "Effective Group ID" (number->string (getegid))))))

(define (get-gc-stats)
  (let ((x (gc-stats)))
    (vector 
     (vector "GC Time Taken" (time-string (/ (assoc-ref x 'gc-time-taken) (exact->inexact internal-time-units-per-second))))
     (vector "Heap Size" (memory-string (assoc-ref x 'heap-size)))
     (vector "Heap Free Size" (memory-string (assoc-ref x 'heap-free-size)))
     (vector "Heap Total Allocated" (memory-string (assoc-ref x 'heap-total-allocated)))
     (vector "Heap Allocated Since GC" (memory-string (assoc-ref x 'heap-allocated-since-gc)))
     (vector "Protected Objects" (number->locale-string (assoc-ref x 'protected-objects)))
     (vector "GC Events" (number->locale-string (assoc-ref x 'gc-times)))
     ;; (vector "GC Live Object Stats" (gc-live-object-stats))
     )))

;; The return value is a vector
;; Each entry is a 2-element vector that is #(category entries)
;; - category is a string
;; - entries is a vector. Each entry is a 2-element vector that is #(key val)
;;   - key is a string
;;   - val is a string
;;
;; Every key and value is guaranteed to be a string, since the C side
;; converts them with scm_to_utf8_string.  Unknown values become "".
(define (->display-string x)
  (cond
   ((string? x) x)
   ((not x) "")
   (else (object->string x))))

(define (sanitize-entries entries)
  (vector-map (lambda (i entry)
                (vector (->display-string (vector-ref entry 0))
                        (->display-string (vector-ref entry 1))))
              entries))

(define (category name thunk)
  "Builds one environment category. If THUNK throws, the category is
shown with a single row describing the error."
  (vector name
          ""
          (sanitize-entries
           (catch #t
             thunk
             (lambda (key . args)
               (vector (vector "error" (format #f "~a ~s" key args))))))))

(define (gdn-get-environment)
  "Lots of info about the current running environment."
  (vector
   (category "system" get-system-information)
   (category "memory" get-gc-stats)
   (category "environment" get-environment-variables)
   (category "locale" get-locale-information)
   (category "process" get-process-information)
   (category "time" get-time-entries)
   (category "user" get-user-information-entries)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
#|
(use-modules
 ((system repl debug) #:select (stack->vector) #:prefix __)
 ((system vm frame) #:select (frame-bindings binding-representation binding-index binding-ref binding-name) #:prefix __)
 ((system vm program) #:select (source:file source:column source:line-for-user) #:prefix __)
 (system vm trap-state)
 (system repl common)
 (system repl debug)
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
    (gdn-update-traps! *gdn-trap-view* (list-traps))
    ;; (run-hook before-read-hook)
    ;; (%gdn-update-thread-info)
    ;; (%gdn-update-environment-info (gdn-get-environment))
    #;(gdn-update-backtrace
     (vector-ref (__stack->vector (make-stack #t
                                              8 ; layers to get out of repl-reader
                                              )) 0))
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

(define gdn-run-trap (add-trap-at-procedure-call! gdn-run-argv))

(define (gdn-run-trap-enable)
  (false-if-exception (enable-trap! gdn-run-trap)))

(define (gdn-run-trap-disable)
  (false-if-exception (disable-trap! gdn-run-trap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; Note that the GC and Sweep handlers are created with the C API

(set! %load-hook gdn-load-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handlers

#;(define (gdn-module-defined-hook directory)
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
    (gdn-add-module category name-str abs-filename)))

;(add-hook! module-defined-hook gdn-add-module)

#|
(define (gdn-exit-hook)
  "Inform Guidance that this repl is about to quit"
  (%gdn-exit-handler))

(add-hook! exit-hook gdn-exit-hook)
|#

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
    (gdn-update-traps! *gdn-trap-view* (list-traps))
    (gdn-update-threads)
    (write *gdn-lisp*) (newline)
    (write (gdn-get-user-input *gdn-lisp*)) (newline)
    ((or reader read) (current-input-port))))

(set! repl-reader gdn-repl-reader)

;; We has to use our own prompting-meta-read because we want to
;; update the tabs for each iteration with debug information

(define (gdn-prompting-meta-read repl)
  (catch #t
    (lambda ()
      (error "in custom prompting-meta-read~%")
      (%gdn-update-thread-info)
      (%gdn-update-environment-info (gdn-get-environment))
      (if (and (repl-debug repl)
               (debug-frames (repl-debug repl)))
          (begin
            (format #t "updating backtrace~%")
            (gdn-update-backtrace
             (debug-frames (repl-debug repl))))
          ;; else
          (begin
            (format #t "not updating backtrace ~A~%" (repl-debug repl))
            (gdn-update-backtrace #f)))
      (repl-reader (lambda () (repl-prompt repl))
                   ((@@ (system repl repl) meta-reader) (repl-language repl) (current-module))))
    (lambda (key . args)
      (case key
        ((quit)
         (apply throw key args))
        (else
         (format (current-output-port) "While reading expression:\n")
         (print-exception (current-output-port) #f key args)
         ;; (flush-all-input)
         *unspecified*)))))

(module-set! (resolve-module '(system repl repl))
             'prompting-meta-read
             gdn-prompting-meta-read)

(write (module-ref (resolve-module '(system repl repl)) 'prompting-meta-read)) (newline)

(define (gdn-start-repl)
  ((@@ (system repl repl) start-repl*) (current-language) #f gdn-prompting-meta-read))

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
  (gdn-update-backtrace frame)
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

|#
