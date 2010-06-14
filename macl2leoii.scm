(module macl2leoii scheme
        (provide translate-file pretty-print-leoii)

        (define *include-file* "ck.thf")
        (define *atoms* '())

        (require parser-tools/yacc
                 parser-tools/lex
                 (prefix-in : parser-tools/lex-sre)
                 syntax/readerr
                 scheme/string
                 scheme/cmdline
                 scheme/match)

        (define-tokens value-tokens (ATOM))
        (define-empty-tokens op-tokens (EOF LPAREN RPAREN NOT BARBAR AMPERAMPER MINUSGREATER SMALLERMINUSGREATER EQUALSGREATER DOT))

        (define-lex-abbrevs
          (comment (:or (:: "//" (:* (:~ #\newline)) #\newline) (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))) ; C style
          ;(comment (:: "(*" (complement (:: any-string "*)" any-string)) "*)")) ; OCaml style
          (lower-letter (:/ "a" "z"))
          (upper-letter (:/ #\A #\Z))
          (digit (:/ "0" "9")))

        (define dsll
          (lexer-src-pos
            ((eof) 'EOF)
            ((:or comment #\newline #\return #\tab #\space #\vtab) (return-without-pos (dsll input-port)))
            ("(" 'LPAREN)
            (")" 'RPAREN)
            ("not" 'NOT)
            ("||" 'BARBAR)
            ("&&" 'AMPERAMPER)
            ("->" 'MINUSGREATER)
            ("<->" 'SMALLERMINUSGREATER)
            ("=>" 'EQUALSGREATER)
            ("." 'DOT)
            ((:: (:+ lower-letter) (:* (:or lower-letter upper-letter "_" digit))) (token-ATOM (string->symbol lexeme)))))

        (define dslp
          (lambda (source-name)
            (parser
              (src-pos)
              (start start)
              (end EOF)
              (tokens value-tokens op-tokens)
              (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                       (raise-read-error
                         (format "unexpected ~a" tok-name)
                         source-name
                         (position-line start-pos)
                         (position-col start-pos)
                         (position-offset start-pos)
                         (- (position-offset end-pos)
                            (position-offset start-pos)))))

              (precs (left SMALLERMINUSGREATER)
                     (right MINUSGREATER)
                     (left EQUALSGREATER)
                     (left BARBAR)
                     (left AMPERAMPER)
                     (left NOT))

              (grammar
                (start (() #f)
                       ((expr-list) (reverse $1)))
                (expr ((simple-expr) $1)
                      ((NOT expr) `(mnot ,$2))
                      ((expr BARBAR expr) `(mor ,$1 ,$3))
                      ((expr AMPERAMPER expr) `(mand ,$1 ,$3))
                      ((expr MINUSGREATER expr) `(mimpl ,$1 ,$3))
                      ((expr SMALLERMINUSGREATER expr) `(miff ,$1 ,$3))
                      ((expr EQUALSGREATER expr) `(mcond ,$1 ,$3)))
                (expr-list ((expr-list DOT expr) (cons `(formula ,$3) $1))
                           ((expr) (list `(formula ,$1)))
                           ((expr-list DOT) $1))
                (simple-expr ((ATOM) (begin
                                       (set! *atoms* (cons $1 *atoms*))
                                       $1))
                             ((LPAREN expr RPAREN) $2))))))

        (define translate
          (lambda (s #:src-name (src-name "current-input-port"))
            (let ((ois (open-input-string s)) (statements '()))
              (port-count-lines! ois)
              (list ((dslp src-name) (lambda () (dsll ois))) (sort (map symbol->string (remove-duplicates *atoms*)) string<?)))))

        (define translate-file
          (lambda (path)
            (call-with-input-file path
                                  (lambda (in)
                                    (translate (port->string in) #:src-name path)))))

        (define counter
          (let ((c 0))
            (lambda ()
              (set! c (+ c 1))
              c)))

        (define pretty-print-leoii
          (lambda (code)
            (cond
              ((symbol? code) (symbol->string code))
              ((number? code) (number->string code))
              (else
                (case (car code)
                  ((formula) (format "thf(f~a, conjecture, ( mvalid @ ~a ))." (counter) (pretty-print-leoii (list-ref code 1))))
                  (else (format "( ~a @ ~a )" (car code) (string-join (map (lambda (e) (pretty-print-leoii e)) (cdr code)) " @ "))))))))

        (define main
          (let ((include-file (make-parameter *include-file*)))
            (command-line
              #:once-each
              (("-i" "--include-file ") i
                                        "File to include"
                                        (include-file i))

              #:args (filename)
              (list filename (include-file)))))

        (match-let* (((list filename include-file) main) ((list code atoms) (translate-file filename)))
                    (display (format "include('~a')." include-file))
                    (newline)
                    (display (string-join (map (lambda (e) (format "thf(~a, type, ( ~a: $i > $o ))." e e)) atoms) "\n"))
                    (newline)
                    (display (string-join (map (lambda (e) (pretty-print-leoii e)) code) "\n"))))
