(import
 (owl toplevel)
 (prefix (owl parse) get-))

(define *input-file* "main.S")
(define *output-file* "main.bin")

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
     (lod . #x0e)
     (exc . #x0f)
     )))

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

(define get-whitespace
  (get-plus! get-single-whitespace))

(define get-symbol
  (get-parses
   ((c1   (get-byte-if (char-match "[a-zA-Z_!@#$^&*]")))
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

(define get-literal
  (get-one-of
   get-char
   get-hex
   get-number))

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


(define (get-macro-declaration get-thing)
  (get-parses
   ((_    (get-word "def" '_))
    (_    get-whitespace)
    (name get-symbol)
    (_    maybe-get-whitespace)
    (_    (get-imm #\())
    (_    maybe-get-whitespace)
    (args (get-symbol-list get-symbol))
    (_    (get-imm #\)))
    (_    maybe-get-whitespace)
    (_    (get-imm #\{))
    (code (get-plus (get-thing get-thing)))
    (_    maybe-get-whitespace)
    (_    (get-imm #\}))
    (_    maybe-get-whitespace))
   (tuple 'defmacro name args code)))

(define get-macro-invocation
  (get-parses
   ((name get-symbol)
    (_    maybe-get-whitespace)
    (_    (get-imm #\())
    (args (get-symbol-list (get-either get-symbol get-literal)))
    (_    (get-imm #\)))
    (_    maybe-get-whitespace))
   (tuple 'macroexpand name args)))

(define get-thing
  (let ((parser (λ (self)
                  (get-parses
                   ((skip maybe-get-whitespace)
                    (val
                     (get-one-of
                      (get-macro-declaration self)
                      get-macro-invocation
                      get-instruction
                      get-byte-list
                      get-label)))
                   val))))
    (parser parser)))

(define parser
  (get-plus! get-thing))

(define data
  (get-parse
   parser
   (str-iter (list->string (file->list *input-file*)))
   'shit))

(when (eq? data 'shit)
  (halt 42))

(define (add* point env name value)
  (put env point (put (get env point empty) name value)))

(define (add-label env name point)
  (add* 'labels env name point))

;; macro = ff of
;;         args=(arg1 ...)
;;         body=(...)
(define (add-macro env name macro)
  (add* 'macros env name macro))

(define (reg? sym_)
  (let ((sym (str sym_)))
    (and
     (=  (string-length sym) 2)
     (=  (string-ref sym 0) #\r)
     (>= (string-ref sym 1) #\a)
     (<= (string-ref sym 1) (+ #\a 8)))))

(define (symbol->static-value sym env)
  (cond
   ((reg? sym)    (- (string-ref (str sym) 1) #\a))
   ((symbol? sym) (if-lets ((v (get (get env 'labels empty) sym #f)))
                    v
                    (error "couldn't find symbol " sym)))
   (else
    sym)))

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

(define (compile point lst env)
  (let loop ((point point)
             (env env)
             (lst lst)
             (acc #n))
    (if (null? lst)
        (resolve acc env)
        (tuple-case (car lst)
          ((label name)
           (loop point (add-label env name point) (cdr lst) acc))
          ((instruction instr a_ b_)
           (if-lets ((v (get *instrs* instr #f))
                     (isiz (if (= v 0) 4 2)))
             (loop
              (+ point isiz)
              env
              (cdr lst)
              (append
               acc
               `(,(λ (env)
                    (let ((a (symbol->static-value a_ env))
                          (b (symbol->static-value b_ env)))
                      (when (and (= v 0) (reg? b_))
                        (print "b (is a reg): " b))
                      ;; (print "a: " a)
                      (if (and (= v 0) (not (reg? b_)))
                          (list v a (>> (band b #xff00) 8) (band b #xff)) ; lit
                          (list (if (= v 0)
                                    1
                                    v)
                                (bior (<< a 4) (band #xf b)))))))))
             (error "unknown opc " (car lst))))
          ((bytes bs)
           (loop (+ point (len bs)) env (cdr lst) (append acc bs)))
          ((defmacro name args body)
           (loop point (add-macro env name (ff 'args args 'body body)) (cdr lst) acc))
          ((macroexpand name args)
           (if-lets ((macro (get (get env 'macros empty) name #f)))
             (let ((code (compile point (replace-symbols (get macro 'body #n) (get macro 'args #n) args) env)))
               (loop (+ point (len code)) env (cdr lst) (append acc code)))
             (error "no such macro " (car lst))))
          (else
           (error "unknown " (car lst)))))))

(let ((data (compile 0 (filter tuple? data) empty)))
  (print "data: " data)
  (list->file data *output-file*))
