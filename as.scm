(import
 (owl toplevel)
 (prefix (owl parse) get-))

(define *instrs*
  (list->ff
   '((lit . #x00)
     (mov . #x01)
     (not . #x02)
     (put . #x03)
     (hlt . #x04)
     (jiz . #x05)
     (mem . #x06)
     (mme . #x07)
     (add . #x08)
     (sub . #x09)
     (mul . #x0a)
     (div . #x0b)
     (and . #x0c)
     (xor . #x0d)
     (ior . #x0e)
     (lsh . #x0f)
     (rsh . #x10)
     (lod . #x11)
     (exc . #x12)
     (dbg . #xff)
     )))

(define-syntax λcurry
  (syntax-rules (())
    ((_ (arg1) . body)
     (λ (arg1) . body))
    ((_ (arg1 . rest) . body)
     (λ (arg1) (λcurry rest . body)))))

(define (capply fn arity lst)
  (cond
   ((= arity 0) fn)
   ((= (len lst) arity)
    (capply (fn (car lst)) (- arity 1) (cdr lst)))
   (else
    (syntax-error "bad arity for funcall " (list fn lst)))))

(define static-eval
  (ff
   '+     (tuple 2 (λcurry (a b) (+ a b)))
   '-     (tuple 2 (λcurry (a b) (- a b)))
   '*     (tuple 2 (λcurry (a b) (* a b)))
   '/     (tuple 2 (λcurry (a b) (/ a b)))
   'print (tuple 1 (λcurry (a) (print a)))
   ))

(define (reg? sym_)
  (let ((sym (str sym_)))
    (and
     (=  (string-length sym) 2)
     (=  (string-ref sym 0) #\r)
     (>= (string-ref sym 1) #\a)
     (<= (string-ref sym 1) (+ #\a 8)))))

(define (unfuck c val)
  (if (tuple? val)
      (c val)
      val))

(define (symbol->static-value sym env fatal?)
  (unfuck
   (λ (v) (symbol->static-value v env fatal?))
   (cond
    ((reg? sym)    (- (string-ref (str sym) 1) #\a))
    ((symbol? sym) (if-lets ((v (or (get (get env 'labels empty) sym #f)
                                    (get (get env 'constants empty) sym #f))))
                     v
                     (if fatal?
                         (error "couldn't find symbol " sym)
                         #f)))
    ((tuple? sym)
     (lets ((v _ ((get env 'compile 'bug) -42 (list sym) env)))
       (car v)))
    (else
     sym))))

(define (get-constant env name fatal?)
  (if-lets ((v (get (get env 'constants empty) name #f)))
    (symbol->static-value v env fatal?)
    (error "cannot resolve constant-like value " name)))

(define (try-evaluate fn arg env)
  (if-lets ((vs (get static-eval fn (tuple #f #f)))
            (arity f vs)
            (arg (map (λ (a) (if (symbol? a) (get-constant env a #t) a)) arg)))
    (capply f arity arg)
    (syntax-error "no such static-eval function " fn)))

(define (char-match regex)
  (let ((rex (string->regex (str "m/^" regex "$/"))))
    (λ (c)
      (rex (string c)))))

(define get-comment
  (get-parses
   ((_    (get-byte-if (C = #\;)))
    (data (get-star! (get-byte-if (λ (x) (not (= x #\newline))))))
    (_    get-byte))
   (tuple 'commentary data)))

(define get-single-whitespace
  (get-one-of!
   (get-byte-if (char-match "[\n\t ]"))
   get-comment))

(define maybe-get-whitespace
  (get-star! get-single-whitespace))

(define (maybe-get-whitespace* ret)
  (get-parses
   ((_ maybe-get-whitespace))
   ret))

(define get-whitespace
  (get-plus! get-single-whitespace))

(define get-symbol
  (get-parses
   ((c1   (get-byte-if (char-match "[a-zA-Z_!@#$^&*/+]|-")))
    (rest (get-star (get-byte-if (char-match "[a-zA-Z_!@#$^&*0-9]|-")))))
   (string->symbol (list->string (cons c1 rest)))))

(define get-label
  (get-parses
   ((sym get-symbol)
    (_   (get-byte-if (C = #\:)))
    (_   maybe-get-whitespace))
   (tuple 'label sym)))

(define get-char
  (get-parses
   ((_ (get-imm #\'))
    (c (get-byte-if I)))
   c))

(define get-hex
  (get-parses
   ((_ (get-word "0x" '_))
    (n (get-plus! (get-byte-if (char-match "[0-9a-fA-F]")))))
   (read (str "#x" (list->string n))))) ; i would never abuse the lisp reader

(define get-number
  (get-parses
   ((bs (get-plus! (get-byte-if (char-match "[0-9]")))))
   (fold (λ (a b) (+ (* a 10) (- b #\0))) 0 bs)))

(define (dirty-compile-lst lst env)
  (let ((compile (get env 'compile 'bug)))
    (fold
     (λ (acc it)
       (append
        acc
        (if (tuple? it)
            (lets ((v _ (compile -42 (list it) env)))
              v)
            (list it))))
     #n
     lst)))

;; sexp = evaluatable
(define (get-sexp get-literal)
  (get-parses
   ((_    (get-imm #\())
    (_    maybe-get-whitespace)
    (fn   (get-either get-symbol (get-literal get-literal)))
    (args (get-star
           (get-parses
            ((_   maybe-get-whitespace)
             (arg (get-either get-symbol (get-literal get-literal)))
             (_   maybe-get-whitespace))
            arg)))
    (_    (get-imm #\))))
   (tuple 'evaluatable (λ (env) (try-evaluate fn (dirty-compile-lst args env) env)))))

(define get-literal
  (let ((parser (λ (self)
                  (get-one-of
                   (get-sexp self)
                   get-char
                   get-hex
                   get-number))))
    (parser parser)))

(define get-instruction
  (get-parses
   ((instr get-symbol)
    (_     get-whitespace)
    (v1    (get-either get-symbol get-literal))
    (_     maybe-get-whitespace)
    (_     (get-imm #\,))
    (_     maybe-get-whitespace)
    (v2    (get-either get-symbol get-literal))
    (_     maybe-get-whitespace))
   (tuple 'instruction instr v1 v2)))

(define get-byte-list
  (get-parses
   ((_  (get-imm #\[))
    (vs (get-star! (get-parses
                    ((_ maybe-get-whitespace)
                     (l get-literal)
                     (_ maybe-get-whitespace))
                    l)))
    (_ (get-imm #\]))
    (_ maybe-get-whitespace))
   (tuple 'bytes vs)))

(define (get-symbol-list match)
  (let ((parser (λ (self)
                  (get-parses
                   ((_ maybe-get-whitespace)
                    ;; (s get-symbol)
                    (s match)
                    (_ maybe-get-whitespace)
                    (rest (get-either
                           (get-parses
                            ((_ (get-imm #\,))
                             (l (self self)))
                            l)
                           (get-epsilon #n))))
                   (cons s rest)))))
    (parser parser)))

(define (get-code-block get-thing)
  (get-parses
   ((_    maybe-get-whitespace)
    (_    (get-imm #\{))
    (code (get-either
           (get-epsilon #n)
           (get-plus get-thing)))
    (_    (get-imm #\}))
    (_    maybe-get-whitespace))
   code))

(define (get-macro-declaration get-thing)
  (get-parses
   ((_    (get-word "def" '_))
    (_    get-whitespace)
    (name get-symbol)
    (_    maybe-get-whitespace)
    (_    (get-imm #\())
    (_    maybe-get-whitespace)
    (args (get-either (get-symbol-list get-symbol) (maybe-get-whitespace* #n)))
    (_    (get-imm #\)))
    (code (get-code-block get-thing)))
   (tuple 'defmacro name args code)))

(define (get-macro-invocation get-thing)
  (get-parses
   ((name get-symbol)
    (_    maybe-get-whitespace)
    (_    (get-imm #\())
    (args (get-either
           (get-symbol-list (get-one-of get-symbol get-literal get-byte-list (get-code-block get-thing)))
           (get-epsilon #n)))
    (_    (get-imm #\)))
    (_    maybe-get-whitespace))
   (tuple 'macroexpand name args)))

(define (get-constant-declaration get-thing)
  (get-parses
   ((_ (get-word "constant" '_))
    (_ get-whitespace)
    (name get-symbol)
    (_ maybe-get-whitespace)
    (_ (get-imm #\=))
    (_ maybe-get-whitespace)
    (val (get-one-of get-byte-list (get-macro-invocation get-thing) get-literal get-symbol)))
   (tuple 'defconstant name val)))

(define (get-unwrap get-thing)
  (get-parses
   ((_   maybe-get-whitespace)
    (_   (get-word "..." '...))
    (sym (get-either get-symbol (get-code-block get-thing)))
    (_   maybe-get-whitespace))
   (tuple 'unwrap sym)))

(define get-thing
  (let ((parser (λ (self)
                  (get-parses
                   ((skip maybe-get-whitespace)
                    (val
                     (get-one-of
                      (get-macro-declaration (self self))
                      (get-code-block (self self))
                      (get-constant-declaration (self self))
                      (get-macro-invocation (self self))
                      get-instruction
                      get-byte-list
                      (get-unwrap (self self))
                      get-label)))
                   val))))
    (parser parser)))

(define parser
  (get-plus! get-thing))

(define (add* point env name value)
  (put env point (put (get env point empty) name value)))

(define (add-label env name point)
  (add* 'labels env name point))

;; macro = ff of
;;         args=(arg1 ...)
;;         body=(...)
(define (add-macro env name macro)
  (add* 'macros env name macro))

(define (add-constant env name value)
  (add* 'constants env name value))

(define (resolve lst env)
  (let loop ((lst lst))
    (if (null? lst)
        #n
        (append (if (function? (car lst))
                    ((car lst) env)
                    `(,(car lst)))
                (loop (cdr lst))))))

(define (walk-replace body sym to)
  (cond
   ((null? body) #n)
   ((tuple? body)
    (list->tuple
     (cons
      (ref body 1)
      (walk-replace (cdr (tuple->list body)) sym to))))
   ((pair? body)
    (cons (walk-replace (car body) sym to)
          (walk-replace (cdr body) sym to)))
   (else
    (if (eq? body sym) to body))))

(define (replace-symbols body s1 s2)
  (when (not (= (len s1) (len s2)))
    (error "bad macro invocation" (list "wanted" s1 "got" s2)))
  (let ((f (λ (body a b)
             (let loop ((body body))
               (if (null? body)
                   ()
                   (let ((l (tuple->list (car body))))
                     (cons
                      (list->tuple (cons (car l) (walk-replace (cdr l) a b)))
                      (loop (cdr body)))))))))
    (fold (λ (a b) (f a (car b) (cdr b))) body (zip cons s1 s2))))

(define (unfatal env)
  (put env 'fatal? #f))

(define (compile point lst env)
  (let loop ((point point)
             (env env)
             (lst lst)
             (acc #n))
    (cond
     ((null? lst)
      (values (resolve acc env) point))
     ((pair? (car lst))
      (lets ((code point* (compile point (car lst) (unfatal env))))
        (loop point* env (cdr lst) (append acc code))))
     (else
      (tuple-case (car lst)
        ((label name)
         (loop point (add-label env name point) (cdr lst) acc))
        ((instruction instr a_ b_)
         (if-lets ((v (get *instrs* instr #f))
                   (isiz (if (and (= v 0) (not (reg? b_))) 4 2)))
           (letrec ((f (λ (env)
                         (lets ((fatal? (get env 'fatal? #f))
                                (a (symbol->static-value a_ env fatal?))
                                (b (symbol->static-value b_ env fatal?)))
                           (if (and a b)
                               (if (and (= v 0) (not (reg? b_)))
                                   (list v a (>> (band b #xff00) 8) (band b #xff)) ; lit
                                   (list (if (= v 0)
                                             1
                                             v)
                                         (bior (<< a 4) (band #xf b))))
                               (list f))))))
             (loop (+ point isiz) env (cdr lst) (append acc (list f))))
           (error "unknown opc " (car lst))))
        ((bytes bs)
         (loop (+ point (len bs)) env (cdr lst)
               (append acc `(,(λ (env) (dirty-compile-lst bs env))))))
        ((defconstant name value)
         (loop point (add-constant env name value) (cdr lst) acc))
        ((evaluatable fn)
         (loop point env (cdr lst) (append acc (list (fn env)))))
        ((defmacro name args body)
         (loop point (add-macro env name (ff 'args args 'body body)) (cdr lst) acc))
        ((macroexpand name args)
         (if-lets ((macro (get (get env 'macros empty) name #f)))
           (lets ((code point*
                  (if (function? (get macro 'body #f))
                      (compile point (capply (get macro 'body 'bug) (len (get macro 'args #n)) args) (unfatal env))
                      (compile point (replace-symbols (get macro 'body #n) (get macro 'args #n) args) (unfatal env)))))
             (loop point* env (cdr lst) (append acc code)))
           (error "no such macro " (car lst))))
        ((unwrap lst*)
         (lets ((code point* (compile point (symbol->static-value lst* env (get env 'fatal? #f)) (unfatal env))))
           (loop point* env (cdr lst) (append acc code))))
        (else
         (error "unknown assembler directive " (car lst))))))))

(define (start-gensym)
  (thread 'gensym (let loop ((n 1))
                    (lets ((who _ (next-mail)))
                      (mail who (string->symbol (str "g" n)))
                      (loop (+ n 1))))))

(define (gensym)
  (interact 'gensym '_))

(define default-env
  (pipe empty
    (put 'compile compile)
    (add-macro 'gensym (ff 'args '(reg)
                           'body (λcurry (reg)
                                   (let ((g (gensym)))
                                     `(,(tuple 'instruction 'lit reg g)
                                       ,(tuple 'label g))))))
    (put 'fatal? #t)
    ))



(λ (args)
  (start-gensym)
  (when (= (len args) 3)
    (lets ((a (list->tuple (cdr args)))
           (input-file output-file a))
      (if-lets ((data (get-try-parse
                       parser
                       (str-iter (list->string (file->list input-file)))
                       input-file
                       "syntax error"
                       (λ arg
                         (print-to stderr "syntax error: " arg)
                         (halt 42)))))
        (begin
          (lets ((data _ (compile 0 (filter tuple? data) default-env)))
            (list->file data output-file))
          (print 'ok)
          (halt 0))
        (syntax-error "syntax error so bad i don't know what to say" #f))))
  (halt 42))
