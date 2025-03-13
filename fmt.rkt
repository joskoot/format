#lang racket

; By Jacob J. A. Koot.

; See fmt.scrbl for documentation.

(provide fmt fmt? fmt-port fmt-clear-hash fmt-hash-size)

;===================================================================================================
; Section 0
; Syntax for definition of procedures that use a hash in order to avoid repeated
; computation when repeatedly called with equal arguments.
; Accepts keyword and optional arguments and rest-argument.
; Does not accept curried definitions.
; When used with equal arguments for procedures with side effects,
; the side effects are produced for the first call only.

; Enable or disable use-hash? to use hashes.
; When disabled, define-hashed expands to a normal define-form.

(define-for-syntax use-hash? #t)

(define-syntax (define-hashed stx)
 (define (get-key xstx)
  (define (get-args stx) ; Extracts the names of all formal arguments.
   (syntax-case stx ()   ; Returns a list of the identifiers.
    ; final case
    (() #'())
    ; aggregating arg
    (id
     (identifier? #'id)
     (list #'id))
    ; required arg
    ((id . rest)
     (identifier? #'id)
     (cons #'id (get-args #'rest)))
    ; optional arg
    (((id dflt) . rest)
     (identifier? #'id)
     (cons #'id (get-args #'rest)))
    ; required keyword arg
    ((kw id . rest)
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     (cons #'id (get-args #'rest)))
    ; optional keyword arg
    ((kw (id dflt) . rest)
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     (cons #'id (get-args #'rest)))
    (else (raise-syntax-error 'define-hashed  "incorrect formal argument" xstx #'else))))
  (syntax-case (get-args xstx) ()
   (() #''())
   ((id) #'id)
   ((id ...) #'(list id ...))))
 (syntax-case stx ()
  ((_ (name . args) . body) (identifier? #'name)
   (if use-hash?
    (with-syntax ((key (get-key #'args)))
   #'(begin
      (define hash (make-hash))
      (set! hash-tables (cons hash hash-tables))
      (define (name . args)
       (define (thunk) . body)
       (hash-ref! hash key thunk))))
  #'(define (name . args) . body)))))

(define hash-tables '())

(define (fmt-clear-hash) (for-each hash-clear! hash-tables))

(define (fmt-hash-size) (apply + (map hash-count hash-tables)))

;===================================================================================================
; Section 1
; Format procedures.
; Every format procedure is a structure with procedure property.
; Procedure   : fmt-proc   : For prop:procedure.
; Constructor : make-fmt   : Raw constructor.
; Constructor : fmt        : Calls make-fmt after checking, parsing and translating arguments.
; Predicate   : fmt?       :  
; Accessor    : fmt-instrs : List of instructions (procedures).
; Accessor    : fmt-port   : Output-port arg.

;---------------------------------------------------------------------------------------------------
; Section 1a
; Constructor fmt.

(define-hashed (fmt . args)
 (define-values (fmt-str port fmts) (check-and-rearrange-args-of-fmt args))
 (make-fmt (fmt-parser fmt-str fmts) port))

(define (check-and-rearrange-args-of-fmt args)
 (define (loop args fmt-str separator port fmts)
  ; args      : args of procedure fmt.
  ; fmt-str   : concatenation of the fmt-strings encountered in args.
  ; separator : separator to be put between fmt-strings encountered in args.
  ; port      : output-port arg found among the args.
  ; fmts      : list of fmt-procs encountered among the args. 
  ; Check that fmt-string args do not contain null-characters outside literals.
  ; Insert null-characters in formed fmt-str where fmt-procs are to be called.
  ; Check that literals are not split over two or more args.
  ; Check that single quotes are balanced.
  (cond
   ((null? args) (values fmt-str (or port 'string) (reverse fmts)))
   (else
    (define arg (car args))
    (define rest (cdr args))
    (cond
     ((fmtstr? arg) ; concatenate partial fmt-strings.
      (loop rest (string-append fmt-str separator arg) "," port fmts))
     ((fmt? arg) ; Insert "\u0000" where an fmt-proc is to be called.
      (loop rest (string-append fmt-str "\u0000") separator port (cons arg fmts)))
     ((or (memq arg '(string current argument str arg cur)) (output-port? arg))
      ; Handle port argument.
      (if port (error 'fmt "multiple port argument: ~s" arg)
       (loop rest fmt-str separator (extend-port-arg arg) fmts))) ; accept arg as port
     (else
      (raise-type-error 'fmt
       "or/c output-port? 'string 'str 'current 'cur 'argument 'arg fmt?" arg))))))
  (loop args "" "" #f '()))

(define (extend-port-arg arg)
 (case arg
  ((str) 'string)
  ((cur) 'current)
  ((arg) 'argument)
  (else arg)))

; Predicate (fmtstr? arg) -> #t if arg is (partial) fmt-str, else #f.
; Check that the string does not contain null-characters outside literals.
; Check that single quotes are balanced.
; A quote while traversing a literal, may end the literal or may be the first
; one of two adjacent quotes within the literal. The parser has to distinguish
; these two cases, but for fmtstr? this is not necessary, because we only check
; the form of the string, not its meaning.

(define (fmtstr? arg)
 (define (fmt-char-list? chars)
  (or (null? chars)
   (case (car chars)
    ((#\null) (null-error arg))
    ((#\') (literal? (cdr chars)))
    (else (fmt-char-list? (cdr chars))))))
 (define (literal? chars)
  (cond
   ((null? chars) (unbalanced-quote-error arg))
   ((char=? (car chars) #\') (fmt-char-list? (cdr chars)))
   (else (literal? (cdr chars)))))
 (and (string? arg) (fmt-char-list? (string->list arg))))

(define (unbalanced-quote-error arg) (error 'fmt "unbalanced quote in ~s" arg))
(define (null-error arg) (error 'fmt "null character is not an fmt instr in: ~s" arg))

;---------------------------------------------------------------------------------------------------
; Section 1b
; The format procedure proper for the procedure property of the underlying fmt
; struct type. Notice that procedure fmt-proc must be defined before the struct
; type can be defined in section 1c.

(define (fmt-proc fmt-struct . the-user-data)
 (define-values (the-port user-data)
  (check-and-extract-args-of-fmt-proc (fmt-port fmt-struct) the-user-data))
 ; Initialize the runtime state.
 (define run-state (make-new-run-time-state user-data))
 ; Determine the output port before excuting the format instructions.
 (define port (if (memq the-port '(cur current)) (current-output-port) (extend-port-arg the-port)))
 (and (output-port? port) (port-closed? port) (error 'fmt "port ~a is closed" port))
 ; Run the format instructions.
 ; Results are gathered in output-string within runtime-state.
 ; Call-with-exit will store the top level exit in runtime-state.
 ; Run-instrs will store the local exit in runtime-state.
 (define (thunk) (run-instrs run-state (fmt-instrs fmt-struct)))
 (call-with-exit run-state run-state-top-exit set-run-state-top-exit! thunk)
 ; Check that all data have been consumed.
 (define remaining-data (run-state-data run-state))
 (when (not (null? remaining-data)) (run-error 1 remaining-data))
 ; Return results as string or send them to output-port.
 (define result (get-output-string (run-state-temp-port run-state)))
 (if (eq? port 'string) result (display result port)))

(define (check-and-extract-args-of-fmt-proc port the-user-data)
 (cond ; checks the args of fmt-proc.
  ((not (memq port '(argument arg))) (values port the-user-data))
  ((pair? the-user-data)
   (define the-port (car the-user-data))
   (define user-data (cdr the-user-data))
   (unless (or (output-port? the-port) (memq the-port '(string current str cur)))
    (raise-type-error 'fmt-proc port-type-string the-port))
   (values the-port user-data))
  (else (raise-type-error 'fmt-proc port-type-string 'none))))

(define port-type-string "or/c output-port? 'current 'cur 'string 'str")

;---------------------------------------------------------------------------------------------------
; Section 1c
; Underlying struct type for fmt-procs.

(define (fmt-printer fmt-struct port write?) (display "#<fmt>" port))

(define-values (fmt-descr make-fmt fmt? fmt-acc fmt-mut)
 (make-struct-type
  'fmt ; name
  #f ; no super struct type
  2 ; nr of fields: list-of-instr-procs and port-argument
  0 ; no auto fields
  #f ; no auto value 
  (list (cons prop:custom-write fmt-printer)) ; property list
  (make-sibling-inspector)
  fmt-proc ; procedure property
  '(0 1) ; both fields are immutable
  #f)) ; no guard

(define fmt-instrs (make-struct-field-accessor fmt-acc 0 'instrs))
(define fmt-port (make-struct-field-accessor fmt-acc 1 'port))

;===================================================================================================
; Section 2
; Run time engine

;---------------------------------------------------------------------------------------------------
; Section 2a
; Run time engine proper.

(define (run-instrs run-state instrs) ; runs a list of format instructions.
 (define (thunk) (loop instrs))
 (define (loop instrs)
  (when (not (null? instrs))
   ((car instrs) run-state)
   (loop (cdr instrs))))
 (call-with-exit run-state run-state-local-exit set-run-state-local-exit! thunk))

;---------------------------------------------------------------------------------------------------
; Section 2b
; Run time state.

(define-struct run-state
 (data local-exit top-exit temp-port tab-offset align fieldwidth sign-mode)
 #:mutable #:omit-define-syntaxes)

(define (make-new-run-time-state user-data)
 (make-run-state
  user-data ; list of remaining data to be formatted.
  #f ; local exit: defined when top level fmt proc or sub-fmt proc is called.
  #f ;   top exit: defined when top level fmt proc is called.
 (open-output-string) ; temporary output-port
  0                   ; initial tab offset.
  no-align            ; initial alignment.
  0                   ; initial fieldwidth.
  ""))                ; initial sign mode (always either "" or "+")

(define (push-data run-state . data)
 (set-run-state-data! run-state
  (append data (run-state-data run-state))))

(define (pop-datum run-state (pred any?) (type-str ""))
 (define data (run-state-data run-state))
 (cond
  ((null? data) (run-error 2))
  (else
   (define datum (car data))
   (set-run-state-data! run-state (cdr data))
   (if (pred datum) datum (run-error 3 datum type-str)))))

(define (peek-datum run-state (pred any?) (type-str ""))
 (define data (run-state-data run-state))
 (cond
  ((null? data) (run-error 2))
  (else
   (define datum (car data))
   (if (pred datum) datum (run-error 3 datum type-str)))))

(define (call-with-exit run-state accessor mutator thunk)
 (define old-exit (accessor run-state))
 (let/ec new-exit (mutator run-state new-exit) (thunk))
 (mutator run-state old-exit))

;===================================================================================================
; Section 3
; Parser : translates a complete fmt-str to a list of instructions.
; The fmt-str already has been checked by procedure fmt.
; Each instruction is a procedure of one argument, namely the run-state.
; All parsing procedures use the parser-state.
; All instrs use the runtime state.

;---------------------------------------------------------------------------------------------------
; Section 3a
; Parsing time state

(define-struct parser-state (str chars fmts) #:mutable #:omit-define-syntaxes)
; str   : The original string is kept for error messages.
; chars : list of characters yet to be parsed.
; fmts  : list of fmt-procedures to be inserted.

(define (push-fmt-chars parser-state . chars)
 (set-parser-state-chars! parser-state
  (append chars (parser-state-chars parser-state))))

(define (peek-fmt-char parser-state)
 (define chars (parser-state-chars parser-state))
 (and (not (null? chars)) (car chars)))

(define (pop-fmt-char parser-state (required #t))
 (define chars (parser-state-chars parser-state))
 (cond
  ((pair? chars)
   (set-parser-state-chars! parser-state (cdr chars))
   (car chars))
  (required (fmt-error parser-state))
  (else #f)))

(define (pop-fmt-proc parser-state)
 (define fmts (parser-state-fmts parser-state))
 (set-parser-state-fmts! parser-state (cdr fmts))
 (car fmts))

;---------------------------------------------------------------------------------------------------
; Section 3b
; The parser proper.

(define-hashed (fmt-parser fmt-str (fmts '())) ; returns a list of procedures
 (parse-fmt-str
  (make-parser-state fmt-str
   (string->list fmt-str) fmts)))

(define (parse-fmt-str parser-state) ; Parses the whole fmt-str.
 (define (loop instrs)
  (define instr (parse-fmt-instr parser-state))
   (if instr (loop (cons instr instrs))
    (reverse instrs)))
 (loop '()))

(define (parse-fmt-instr parser-state) ; Parses one single instruction.
 (skip-white-fmt-and-commas parser-state)
 (let ((char (pop-fmt-char parser-state #f)))
  (case char
   ((#f) #f) ; signals that no more instructions follow.
   ; Cases that need more parsing call an instruction-parser proc.
   ; Cases that need no more parsing immediately return the related instruction.
   ((#\# #\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
    (push-fmt-chars parser-state char)
    (parse-repeat-instr parser-state))
   ((#\null) (include-fmt (pop-fmt-proc parser-state)))
   ((#\!) (parse-if-more-data-instr parser-state))
   ((#\?) (parse-if-no-more-data-instr parser-state))
   ((#\+) set-sign-mode-instr)
   ((#\-) clear-sign-mode-instr)
   ((#\$) (parse-retain-sign-instr parser-state))
   ((#\') (parse-literal-instr parser-state))
   ((#\^) (parse-special-literal-instr parser-state))
   ((#\() (parse-compound-instr parser-state #\)))
   ((#\[) (parse-special-compound-instr parser-state))
   ((#\*) (parse-indefinite-repeat-instr parser-state))
   ((#\/) newline-instr)
   ((#\|) newline-but-not-double)
   ((#\:) local-exit-instr)
   ((#\;) top-exit-instr)
   ((#\@) (parse-retain-tab-offset-instr parser-state))
   ((#\&) eol-tab-instr)
   ((#\>) (parse-rel-forward-tab-instr parser-state))
   ((#\<) (parse-rel-backward-tab-instr parser-state))
   ((#\~) remaining-data-instr)
   ((#\_) (parse-iteration-instr parser-state))
   ((#\λ) call-proc-instr)
   ((#\%) numerator/denominator-instr)
   ((#\\) magnitude/angle-instr)
   ((#\A #\a) (parse-retain-align-instr parser-state))
   ((#\B #\b) binary-num-instr)
   ((#\C #\c) (parse-align-instr parser-state centre-align))
   ((#\D #\d) display-instr)
   ((#\E #\e) (parse-e-fmt-instr parser-state))
   ((#\F #\f) (parse-float-instr parser-state))
   ((#\G #\g) date-instr)
   ((#\H #\h) hex-num-instr)
   ((#\I #\i) (parse-int-instr parser-state))
   ((#\J #\j) void)
   ((#\{) (parse-jump-instr parser-state))
   ((#\K #\k) call-fmt-instr)
   ((#\L #\l) (parse-align-instr parser-state left-align))
   ((#\M #\m) (parse-retain-state-instr parser-state))
   ((#\N #\n) no-alignment-instr)
   ((#\O #\o) octal-num-instr)
   ((#\=) decimal-num-instr)
   ((#\P #\p) print-instr)
   ((#\Q #\q) (parse-if-datum-instr parser-state))
   ((#\R #\r) (parse-align-instr parser-state right-align))
   ((#\S #\s) skip-instr)
   ((#\T #\t) (parse-tab-instr parser-state))
   ((#\U #\u) unfold-instr)
   ((#\V #\v) recursive-unfold-instr)
   ((#\W #\w) write-instr)
   ((#\X #\x) space-instr)
   ((#\Y #\y) unfold-complex-instr)
   ((#\Z #\z) unfold-all-instr)
   (else (fmt-error parser-state)))))

;---------------------------------------------------------------------------------------------------
; Section 3c
; Alignment instruction parser (instructions L, R and C)

(define (parse-align-instr parser-state aligner)
 (define numeric-arg-proc (parse-numeric-arg parser-state))
 (λ (run-state)
  (set-run-state-align! run-state aligner)
  (set-run-state-fieldwidth! run-state (numeric-arg-proc run-state))))

;---------------------------------------------------------------------------------------------------
; Section 3d
; Numerical format instruction parsers (instruction I, F and E)

(define-syntax def-num-fmt-instr
 (syntax-rules ()
  ((_ name instr m n ...)
   (define (name parser-state)
    (define m (parse-numeric-arg parser-state))
    (define n (parse-numeric-arg parser-state)) ...
    (λ (run-state)
     (let
      ((m (m run-state))
       (n (n run-state)) ...
       (datum (pop-datum run-state real? "real")))
      (display
       (allign-right
        (check-inf/nan datum (λ () (instr run-state datum n ...)))
        m)
       (run-state-temp-port run-state))))))))

(def-num-fmt-instr parse-int-instr   fmt-int  m n)
(def-num-fmt-instr parse-float-instr fmt-real m n)
(def-num-fmt-instr parse-e-fmt-instr fmt-e    m n k)

;---------------------------------------------------------------------------------------------------
; Section 3e
; Conditional instruction parsers (instructions !, ? , _, Q and {ξ ...+})

(define (parse-if-more-data-instr parser-state)
 (define instr (parse-fmt-instr parser-state))
 (when (not instr) (fmt-error parser-state))
 (λ (run-state)
  (when (not (null? (run-state-data run-state))) (instr run-state))))

(define (parse-if-no-more-data-instr parser-state)
 (define instr (parse-fmt-instr parser-state))
 (when (not instr) (fmt-error parser-state))
 (λ (run-state)
  (when (null? (run-state-data run-state))
   (instr run-state))))

(define (parse-if-datum-instr parser-state)
 (define then (parse-fmt-instr parser-state))
 (define else (parse-fmt-instr parser-state))
 (when (not else) (fmt-error parser-state))
 (λ (run-state)
  (if (peek-datum run-state)
   (then run-state)
   (else run-state))))

(define (parse-jump-instr parser-state)
 (define instrs (list->vector (parse-compound-instr-helper parser-state #\})))
 (define m (vector-length instrs))
 (when (zero? m) (fmt-error parser-state))
 (λ (run-state)
  (let ((n (pop-datum run-state exact-nonnegative-integer? "exact-nonnegative-integer")))
   (if (>= n m) (run-error 6 n m)
    ((vector-ref instrs n) run-state)))))

;---------------------------------------------------------------------------------------------------
; Section 3f
; Iteration instruction parsers (instructions *, # and number)

(define (parse-indefinite-repeat-instr parser-state)
 (define instr (parse-fmt-instr parser-state))
 (when (not instr) (fmt-error parser-state))
 (λ (run-state)
  (define (loop)
   (when (not (null? (run-state-data run-state)))
    (instr run-state)
    (loop)))
  (loop)))

(define (parse-repeat-instr parser-state)
 (define get-n (parse-numeric-arg parser-state))
 (define instr (parse-fmt-instr parser-state))
 (when (not instr) (fmt-error parser-state))
 (λ (run-state)
  (define (loop n)
   (when (> n 0)
    (instr run-state)
    (loop (sub1 n))))
  (loop (get-n run-state))))

;---------------------------------------------------------------------------------------------------
; Section 3g
; Tabulation instruction parsers (instructions T, > and <)

(define (parse-tab-instr parser-state)
 (define n (parse-numeric-arg parser-state))
 (λ (run-state)
  (define p (run-state-temp-port run-state))
  (define current-length (string-length (get-output-string p)))
  (define new-pos (+ (run-state-tab-offset run-state) (n run-state)))
  (cond
   ((> new-pos current-length)
    (file-position p current-length)
    (display (make-string (- new-pos current-length) #\space) p))
   (else (file-position p new-pos)))))

(define (parse-rel-forward-tab-instr parser-state)
 (parse-rel-tab-instr parser-state +))

(define (parse-rel-backward-tab-instr parser-state)
 (parse-rel-tab-instr parser-state -))

(define (parse-rel-tab-instr parser-state sense)
 (define n (parse-numeric-arg parser-state))
 (λ (run-state)
  (define p (run-state-temp-port run-state))
  (define nn (sense (file-position p) (n run-state)))
  (when (< nn (run-state-tab-offset run-state)) (run-error 4 n))
  (define len (string-length (get-output-string p)))
  (cond
   ((<= nn len)
    (file-position p nn))
   (else
    (file-position p len)
    (display (make-string (- n len) #\space) p)))))

;---------------------------------------------------------------------------------------------------
; Section 3h
; Retain state instruction parsers (instruction A, $, @ and M)

(define-syntax define-parse-retain-instr
 (syntax-rules ()
  ((_ name field ...)
   (define (name parser-state)
    (define instr (parse-fmt-instr parser-state))
    (when (not instr) (fmt-error parser-state))
    (λ (run-state)
     (define field (get-field field run-state)) ...
     (instr run-state)
     (reset-field field run-state) ...)))))

(define-syntax (get-field stx)
 (syntax-case stx ()
  ((_ field run-state)
 #`(#,(datum->syntax
  #'here
    (string->symbol
     (string-append "run-state-"
      (symbol->string (syntax->datum #'field)))))
    run-state))))

(define-syntax (reset-field stx)
 (syntax-case stx ()
  ((_ field run-state)
 #`(#,(datum->syntax
   #'here
     (string->symbol
      (string-append "set-run-state-"
       (symbol->string (syntax->datum #'field))
       "!")))
    run-state field))))

(define-parse-retain-instr parse-retain-align-instr align fieldwidth)
(define-parse-retain-instr parse-retain-sign-instr sign-mode)
(define-parse-retain-instr parse-retain-tab-offset-instr tab-offset)

(define-parse-retain-instr parse-retain-state-instr
 tab-offset sign-mode align fieldwidth)

;---------------------------------------------------------------------------------------------------
; Section 3i
; Literal instructions.

(define (parse-literal-string parser-state)
 (define (loop chars)
  (define char (pop-fmt-char parser-state))
  (cond
   ((not (char=? char #\')) (loop (cons char chars)))
   (else
    (define peek (peek-fmt-char parser-state))
    (if (and peek (char=? peek #\'))
     (loop (cons (pop-fmt-char parser-state) chars))
     (apply string (reverse chars))))))
 (loop '()))

(define (parse-literal-instr parser-state)
 (define str (parse-literal-string parser-state))
 (λ (run-state)
  (push-data run-state str)
  (display-instr run-state)))

(define (parse-special-literal-instr parser-state)
 (skip-white-fmt-and-commas parser-state)
 (unless (char=? (pop-fmt-char parser-state) #\') (fmt-error parser-state))
 (define p (open-input-string (parse-literal-string parser-state)))
 (define (loop data)
  (define datum (literal-reader p))
   (cond
    ((eof-object? datum)
     (define rdata (reverse data))
     (λ (run-state) (apply push-data run-state rdata)))
    (else (loop (cons datum data)))))
 (loop '()))

(define (literal-reader p)
 (parameterize ((uncaught-exception-handler literal-read-exn))
  (read p)))

(define (literal-read-exn-fmt msg)
 (format "fmt, incorrect datum in ^ instruction: ~s" msg))

(define (literal-read-exn exn)
 ((error-display-handler) (literal-read-exn-fmt (exn-message exn)) exn)
 ((error-escape-handler)))

;---------------------------------------------------------------------------------------------------
; Section 3j
; Compound instructions.

(define (parse-compound-instr parser-state terminator)
 (define instrs (parse-compound-instr-helper parser-state terminator))
 (λ (run-state) (run-instrs run-state instrs)))

(define (parse-compound-instr-helper parser-state terminator)
 (define (loop instrs )
  (skip-white-fmt-and-commas parser-state)
  (define char (pop-fmt-char parser-state))
  (cond
   ((char=? char terminator) (reverse instrs))
   (else
    (push-fmt-chars parser-state char)
    (loop (cons (parse-fmt-instr parser-state) instrs)))))
 (loop '()))

(define (parse-special-compound-instr parser-state)
 (define instr (parse-compound-instr parser-state #\]))
 (λ (run-state)
  (define temp-port (run-state-temp-port run-state))
  (define tab-offset (run-state-tab-offset run-state))
  (set-run-state-temp-port! run-state (open-output-string))
  (set-run-state-tab-offset! run-state 0)
  (instr run-state)
  (push-data run-state (get-output-string (run-state-temp-port run-state)))
  (set-run-state-temp-port! run-state temp-port)
  (set-run-state-tab-offset! run-state tab-offset)))

;---------------------------------------------------------------------------------------------------
; Section 3k
; Parser for numerical argument.

(define (parse-numeric-arg parser-state)
 (skip-white-fmt parser-state)
 (define char (peek-fmt-char parser-state))
 (cond
  ((not char) (λ (run-state) 0))
  ((char=? char #\#)
   (pop-fmt-char parser-state)
   (λ (run-state)
    (pop-datum run-state exact-nonnegative-integer? "natural number")))
  (else
   (define (loop n char )
    (cond
     ((not char) (λ (run-state) n))
     ((char=? char #\.) (pop-fmt-char parser-state) (λ (run-state) n))
     ((char-numeric? char)
      (pop-fmt-char parser-state)
      (loop
       (+ (* 10 n) (string->number (string char)))
       (peek-fmt-char parser-state)))
     (else (λ (run-state) n))))
   (loop 0 char))))

;---------------------------------------------------------------------------------------------------
; Section 3l
; Auxiliary procs for parser

(define (fmt-error parser-state)
 (let ((chars (parser-state-chars parser-state))
       (str (parser-state-str parser-state)))
  (error 'fmt "incorrect format instruction at position ~s in format ~s"
   (- (string-length str) (length chars) 1) str)))

(define (skip-white-fmt parser-state)
 (define (loop chars)
  (if (and (not (null? chars)) (char-whitespace? (car chars)))
   (loop (cdr chars))
   (set-parser-state-chars! parser-state chars)))
 (loop (parser-state-chars parser-state)))

(define (skip-white-fmt-and-commas parser-state)
 (define (loop chars)
  (if
   (and
    (not (null? chars))
    (let ((char (car chars)))
     (or (char=? char #\,) (char-whitespace? char))))
   (loop (cdr chars))
   (set-parser-state-chars! parser-state chars)))
 (loop (parser-state-chars parser-state)))

;---------------------------------------------------------------------------------------------------
; Section 3m
; Include fmt

(define (include-fmt fmt)
 (λ (run-state) (run-instrs run-state (fmt-instrs fmt))))

;===================================================================================================
; Section 4
; Instructions that do not need further parsing.
;---------------------------------------------------------------------------------------------------
; Section 4a
; Display/write/print instructions (D, W, P and J)

(define (display-instr run-state)
 (when (not (eq? (run-state-align run-state) no-align))
  (let ((datum (pop-datum run-state)))
   (push-data run-state
    (if (string? datum)
     (string-trim datum #rx" *")
     datum))))
 (printer run-state display))

(define (write-instr run-state) (printer run-state write)) ; Instruction W.
(define (print-instr run-state) (printer run-state print)) ; Instruction P.

(define (printer run-state instr)
 (let ((datum (pop-datum run-state)) (out-str (open-output-string)))
  (instr datum out-str)
  (display
   ((run-state-align run-state)
    (get-output-string out-str)
    (run-state-fieldwidth run-state))
   (run-state-temp-port run-state))))

;---------------------------------------------------------------------------------------------------
; Section 4b
; No alignment instruction N.

(define (no-alignment-instr run-state)
 (set-run-state-align! run-state no-align)
 (set-run-state-fieldwidth! run-state 0))

;---------------------------------------------------------------------------------------------------
; Section 4c
; Numerical formats (instructions B, O, H = and `)

(define (binary-num-instr run-state) (num-instr-with-base run-state  2))
(define ( octal-num-instr run-state) (num-instr-with-base run-state  8))
(define (decimal-num-instr run-state) (num-instr-with-base run-state 10))
(define (   hex-num-instr run-state) (num-instr-with-base run-state 16))

;---------------------------------------------------------------------------------------------------
; Section 4d
; Tabulation instruction &.

(define (eol-tab-instr run-state)
 (let ((p (run-state-temp-port run-state)))
  (file-position p eof #;(string-length (get-output-string p)))))

;---------------------------------------------------------------------------------------------------
; Section 4e
; Unfolders (instructions U, V, Y and Z)

(define (unfold-instr run-state)
 (let ((data (unfold (pop-datum run-state))))
  (apply push-data run-state (length data) data)))

(define (unfold datum)
 (cond
  ((list? datum) datum)
  ((vector? datum) (vector->list datum))
  ((struct? datum) (vector->list (struct->vector datum)))
  ((box? datum)  (list (unbox datum)))
  ((or (pair? datum) (mpair? datum)) (unfold-pair datum (hasheq)))
  (else (list datum))))

(define (unfold-pair datum hash)
 (cond
  ((hash-has-key? hash datum)
   (list datum))
  ((null? datum) '())
  ((pair? datum)
   (cons (car datum) (unfold-pair (cdr datum) (hash-set hash datum #t))))
  ((mpair? datum)
   (cons (mcar datum) (unfold-pair (mcdr datum) (hash-set hash datum #t))))
  (else (list datum))))

(define (recursive-unfold-instr run-state)
 (let ((data (recursively-unfold (pop-datum run-state) (hasheq))))
  (apply push-data run-state (length data) data)))

(define (unfold-all-instr run-state)
 (let ((data (recursively-unfold (run-state-data run-state) (hasheq))))
  (set-run-state-data! run-state (cons (length data) data))))

(define (recursively-unfold datum hash)
 (cond
  ((hash-has-key? hash datum)
   (list datum))
  ((null? datum) '())
  ((pair? datum)
   (let ((hash (hash-set hash datum #t)))
    (append
     (recursively-unfold (car datum) hash)
     (recursively-unfold (cdr datum) hash))))
  ((mpair? datum)
   (let ((hash (hash-set hash datum #t)))
    (append
     (recursively-unfold (mcar datum) hash)
     (recursively-unfold (mcdr datum) hash))))
  ((vector? datum)
   (recursively-unfold (vector->list datum) (hash-set hash datum #t)))
  ((struct? datum)
   (recursively-unfold (struct->vector datum) (hash-set hash datum #t)))
  ((box? datum)
   (recursively-unfold (unbox datum) (hash-set hash datum #t)))
  (else (list datum))))

(define (unfold-complex-instr run-state)
 (let ((datum (pop-datum run-state number? "number")))
  (push-data run-state (real-part datum) (imag-part datum))))

(define (parse-iteration-instr parser-state)
 (let
  ((n (parse-numeric-arg parser-state))
   (m (parse-numeric-arg parser-state))
   (instr0 (parse-fmt-instr parser-state))
   (instr1 (parse-fmt-instr parser-state)))
  (unless instr1 (fmt-error parser-state))
  (λ (run-state)
   (let ((n (n run-state)) (m (m run-state)))
     (for ((k (in-range 0 (quotient n m))))
      (for ((l (in-range 0 m))) (instr0 run-state))
      (instr1 run-state))
     (for ((k (in-range 0 (remainder n m)))) (instr0 run-state))))))

;---------------------------------------------------------------------------------------------------
; Section 4f
; Miscelaneous instructions (X, :, ;, S, +, -, /, |)

(define (space-instr run-state) (display " " (run-state-temp-port run-state)))
(define (local-exit-instr run-state) ((run-state-local-exit run-state) (void)))
(define (top-exit-instr run-state) ((run-state-top-exit run-state) (void)))
(define (skip-instr run-state) (void (pop-datum run-state)))

(define (clear-sign-mode-instr run-state)
 (set-run-state-sign-mode! run-state ""))

(define (set-sign-mode-instr run-state)
 (set-run-state-sign-mode! run-state "+"))

(define (newline-instr run-state)
 (let ((p (run-state-temp-port run-state)))
  (newline p)
  (set-run-state-tab-offset! run-state (file-position p))))

(define (newline-but-not-double run-state)
 (unless
  (=
   (file-position (run-state-temp-port run-state))
   (run-state-tab-offset run-state))
  (newline-instr run-state)))

;---------------------------------------------------------------------------------------------------
; Section 4g
; Date/time instruction (G)

(define (date-instr run-state)
 (let*-values
  (((week-day day month year hour minute second time-zone)
    (apply values (date-time-components run-state)))
   ((week-day) (vector-ref week-days week-day))
   ((month) (vector-ref months month))
   ((tzh tzm) (quotient/remainder (round (/ time-zone 60)) 60))
   ((tzm) (abs tzm)))
  (push-data run-state week-day day month (modulo year 10000) hour minute second tzh tzm)
  (date-instr-fmt run-state)))

(define date-time-components
 (let*
  ((selectors
    (list
     date-week-day
     date-day
     date-month
     date-year
     date-hour
     date-minute
     date-second
     date-time-zone-offset)))
  (λ (run-state)
   (let*
    ((datum
      (pop-datum run-state natural-or-false? "natural number or false"))
     (dummy
      (when (and datum (>= datum (expt 2 32)))
       (error 'fmt "too large argument for instruction G: ~s" datum)))
     (date/time (seconds->date (or datum (current-seconds)))))
    (map (λ (selector) (selector date/time)) selectors)))))

(define week-days #(Sun Mon Tue Wed Thu Fri Sat))
(define months #(#f Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))

;---------------------------------------------------------------------------------------------------
; Section 4h
; Readers: haas been eliminated.

;(define (read-instr run-state) ; Instruction =.
; (let ((p (pop-datum run-state input-port? "input port")))
;  (let ((datum (reader p)))
;   (if (eof-object? datum) (push-data run-state #f)
;    (push-data run-state #t datum)))))
;
;(define (read-all-instr run-state) ; Instruction J.
; (let ((p (pop-datum run-state input-port? "input port")))
;  (let loop ((n 0) (lst '()))
;   (let ((datum (reader p)))
;    (if (eof-object? datum) (apply push-data run-state n (reverse lst))
;     (loop (add1 n) (cons datum lst)))))))
;
;(define (reader p)
; (parameterize ((uncaught-exception-handler read-exn))
;  (read p)))
;
;(define (read-exn-fmt msg)
; (format "fmt: read error in = or J instr. ~a" msg))
;
;(define (read-exn exn)
; ((error-display-handler) (read-exn-fmt (exn-message exn)) exn)
; ((error-escape-handler)))

;---------------------------------------------------------------------------------------------------
; Section 4i
; Instructions K and λ.

(define (call-fmt-instr run-state)
 (let ((datum (pop-datum run-state fmt-or-str? "fmt or fmt-string")))
  (let ((instrs ((if (string? datum) fmt-parser fmt-instrs) datum)))
   (run-instrs run-state instrs))))

(define (call-proc-instr run-state)
 (let ((proc (pop-datum run-state proc-with-arity-1? "proc with arity 1")))
  (let
   ((data
     (call-with-continuation-barrier
      (λ ()
       (call-in-nested-thread
        (λ ()
         (proc (run-state-data run-state))))))))
   (if (list? data) (set-run-state-data! run-state data)
    (run-error 5 data)))))

; The continuation barrier in call-proc-instr is installed because it makes
; no sense to reenter the fmt-proc later by means of a continuation made by
; the called proc. The state of the fmt-proc may have been mutated.

;---------------------------------------------------------------------------------------------------
; Section 4e
; Unfolders (instruction % and φ)

(define (numerator/denominator-instr run-state)
 (let*
  ((datum (pop-datum run-state rational? "rational"))
   (datum (inexact->exact datum)))
  (push-data run-state (numerator datum) (denominator datum))))

(define (magnitude/angle-instr run-state)
 (let* ((datum (pop-datum run-state number? "number")))
  (if (and (exact? datum) (zero? datum))
   (push-data run-state 0 0)
   (push-data run-state (magnitude datum) (angle datum)))))

;===================================================================================================
; Section 5
; Auxiliary procedures for numerical formats.

(define (num-instr-with-base run-state base)
 (let*
  ((datum (pop-datum run-state real? "real"))
   (sign (get-sign run-state datum)))
  (display
   ((run-state-align run-state)
    (check-inf/nan
     datum
     (λ ()
      (string-append sign
       (number->string (abs (inexact->exact datum)) base))))
    (run-state-fieldwidth run-state))
   (run-state-temp-port run-state))))

(define (fmt-int run-state datum n)
 (let ((sign (get-sign run-state datum))
       (datum (number->string (round (abs (inexact->exact datum))))))
  (string-append sign (allign-right datum n #\0))))

(define (fmt-real run-state datum n)
 (let ((sign (get-sign run-state datum)) (datum (abs datum)))
  (string-append sign (real->decimal-string datum n))))

(define (fmt-e run-state datum n k)
 (let* ((sign (get-sign run-state datum)) (datum (abs (inexact->exact datum))))
  (if (zero? datum) (zero-e-fmt sign n k)
   (let*-values
    (((n+1) (add1 n))
     ((exp) (order-of-magnitude datum))
     ((datum) (* datum (expt 10 (- n exp))))
     ((rounded-datum) (round datum))
     ((rounded-datum exp)
      (if (>= rounded-datum (expt 10 n+1))
          (values (round (* datum 1/10)) (add1 exp))
          (values rounded-datum exp)))
     ((datum) (number->string rounded-datum))
     ((int-part) (substring datum 0 1))
     ((fraction) (substring datum 1 n+1))
     ((exp)
      (let ((sign-mode (run-state-sign-mode run-state)))
           (set-run-state-sign-mode! run-state "+")
           (begin0 (fmt-int run-state exp k)
                   (set-run-state-sign-mode! run-state sign-mode)))))
    (string-append sign int-part "." fraction "e" exp)))))

; Auxiliaries for format e-fmt

#;(define order-of-magnitude
 (let*
  ((exact-log (λ (x) (inexact->exact (log x))))
   (inverse-exact-log10 (/ (exact-log 10))))
  (λ (r)
   (unless (and (rational? r) (positive? r))
    (raise-type-error 'order-of-magnitude
     "positive real number but not ±inf.0 nor ±nan.0" r))
   (let ((q (inexact->exact r)))
    (let
     ((m
       (floor
        (* (- (exact-log (numerator q)) (exact-log (denominator q)))
         inverse-exact-log10))))
     (let ((p (expt 10 m)))
      (if (< q p)
       (let loop ((m (sub1 m)) (p (* p 1/10)))
        (if (< q p) (loop (sub1 m) (* p 1/10)) m))
       (let loop ((m m) (p (* p 10)))
        (if (>= q p) (loop (add1 m) (* p 10)) m)))))))))

(define (zero-e-fmt sign n k)
 (string-append
  sign
  "0."
  (make-string n #\0)
  "e+"
  (make-string (max 1 k) #\0)))

; Other auxiliaries for numeric formats

(define (check-inf/nan datum thunk)
 (cond
  ((eqv? datum +inf.0) "+inf.0")
  ((eqv? datum -inf.0) "-inf.0")
  ((eqv? datum +nan.0) "+nan.0")
  ((eqv? datum -nan.0) "-nan.0")
  (else (thunk))))

(define (get-sign run-state datum)
 (cond
  ((negative? datum) "-")
  ((eqv? datum -0.0) "-")
  (else (run-state-sign-mode run-state))))

;===================================================================================================
; Section 6
; Auxiliary procedures for instructions.

(define (natural-or-false? x) (or (not x) (exact-nonnegative-integer?  x)))
(define (fmt-or-str? x) (or (fmtstr? x) (fmt? x)))
(define (any? x) #t)

(define (proc-with-arity-1? p)
 (and (procedure? p) (procedure-arity-includes? p 1)))

(define (allign-right str m (char #\space))
 (string-append (make-string (max 0 (- m (string-length str))) char) str))

(define (allign-left str m (char #\space))
 (string-append str (make-string (max 0 (- m (string-length str))) char)))

(define (no-align str n) str)
(define (left-align str n) (allign-left str n))
(define (right-align str n) (allign-right str n))

(define (centre-align str n)
 (let ((n (/ (max 0 (- n (string-length str))) 2)))
  (string-append
   (make-string (ceiling n) #\space)
   str
   (make-string (floor n) #\space))))

(define run-error
 (let ((err (λ x (apply error 'fmt x))))
  (λ (n . args)
   (case n
    ((1) (err "the following data are left over at end of fmt: ~s" (car args)))
    ((2) (err "more data expected than actually given"))
    ((3) (raise-type-error 'fmt (cadr args) (car args)))
    ((4) (err "tab instr < results in negative position: ~s" (car args)))
    ((5) (err "λ instr did not return a list. got: ~s" (car args)))
    ((6) (err "J-instr: index must be less than ~s, given ~s" (cadr args) (car args)))
    (else (error 'fmt "system error: proc run-error lacks a case."))))))

;===================================================================================================
; Section 7
; The following two procedures come last, because their definition calls
; procedure fmt-parser and therefore requires almost all stuff already to be
; defined and assigned!

(define remaining-data-instr ; instr ~
 (let ((instrs (fmt-parser "!(&n*(w!x)/)")))
  (λ (run-state) (run-instrs run-state instrs))))

(define date-instr-fmt ; for instr G
 (let
  ((instrs
    (fmt-parser "M(N-D','XI2.2XDXI4.4XI2.2':'I2.2':'I2.2X+I2.2-I2.2)")))
  (λ (run-state) (run-instrs run-state instrs))))

(fmt-clear-hash)

;===================================================================================================
; The end
