(module macl2leoii scheme
        (provide translate-file pretty-print-leoii)

        (define *atoms* '())

        (require parser-tools/yacc
                 parser-tools/lex
                 (prefix-in : parser-tools/lex-sre)
                 syntax/readerr
                 scheme/string
                 scheme/cmdline
                 scheme/match)

        (define-tokens value-tokens (ATOM))
        (define-empty-tokens op-tokens (EOF LPAREN RPAREN NOT AMPERAMPER BARBAR MINUSGREATER EQUALSGREATER DOT))

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
            ("&&" 'AMPERAMPER)
            ("||" 'BARBAR)
            ("->" 'MINUSGREATER)
            ("=>" 'EQUALSGREATER)
            ("." 'DOT)
            ((:: (:+ lower-letter) (:* (:or lower-letter upper-letter "_" digit))) (token-ATOM (string->symbol lexeme)))))

        (define dslp
          (lambda (source-name selection-function)
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

              (precs (right MINUSGREATER)
                     (left EQUALSGREATER)
                     (left BARBAR)
                     (left AMPERAMPER)
                     (left NOT))

              (grammar
                (start (() #f)
                       ((expr-list) (reverse $1)))
                (expr ((simple-expr) $1)
                      ((NOT expr) (let ((x (gensym "X")))
                                    `(lambda ,x (not (app ,$2 ,x)))))
                      ((expr AMPERAMPER expr) (let ((x (gensym "X")))
                                                `(lambda ,x (and (app ,$1 ,x) (app ,$3 ,x)))))
                      ((expr BARBAR expr) (let ((x (gensym "X")))
                                            `(lambda ,x (or (app ,$1 ,x) (app ,$3 ,x)))))
                      ((expr MINUSGREATER expr) (let ((x (gensym "X")))
                                                  `(lambda ,x (or (not (app ,$1 ,x)) (app ,$3 ,x)))))
                      ((expr EQUALSGREATER expr) (let ((x (gensym "X"))
                                                       (w (gensym "W")))
                                                   `(lambda ,x (forall ,w (implies (app (app ,selection-function ,x ,$1) ,w) (app ,$3 ,w)))))))
                (expr-list ((expr-list DOT expr) (cons `(formula ,$3) $1))
                           ((expr) (list `(formula ,$1)))
                           ((expr-list DOT) $1))
                (simple-expr ((ATOM) (begin
                                       (set! *atoms* (cons $1 *atoms*))
                                       $1))
                             ((LPAREN expr RPAREN) $2))))))

        (define translate
          (lambda (s selection-function #:src-name (src-name "current-input-port"))
            (let ((ois (open-input-string s)) (statements '()))
              (port-count-lines! ois)
              (list ((dslp src-name selection-function) (lambda () (dsll ois))) (reverse (remove-duplicates *atoms*)))))) ; sort instead of reverse?

        (define translate-file
          (lambda (path selection-function)
            (call-with-input-file path
                                  (lambda (in)
                                    (translate (port->string in) selection-function #:src-name path)))))

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
                  ((and) (format "( ~a & ~a )" (pretty-print-leoii (list-ref code 1)) (pretty-print-leoii (list-ref code 2))))
                  ((app) (format "( ~a )" (string-join (map (lambda (e) (pretty-print-leoii e)) (cdr code)) " @ ")))
                  ((forall) (format "( ! [~a: $i] : ~a )" (list-ref code 1) (pretty-print-leoii (list-ref code 2))))
                  ((formula) (format "thf(f~a, conjecture, ( ~a ))." (counter) (pretty-print-leoii (list-ref code 1))))
                  ((implies) (format "( ~a => ~a )" (pretty-print-leoii (list-ref code 1)) (pretty-print-leoii (list-ref code 2))))
                  ((lambda) (format "( ^ [~a: $i] : ~a )" (list-ref code 1) (pretty-print-leoii (list-ref code 2))))
                  ((not) (format "~~ ~a" (pretty-print-leoii (list-ref code 1))))
                  ((or) (format "( ~a | ~a )" (pretty-print-leoii (list-ref code 1)) (pretty-print-leoii (list-ref code 2)))))))))

        (define main
          (let ((selection-function (make-parameter 'f)))
            (command-line
              #:once-each
              (("-f" "--selection-function ") sf
                                              "Selection function: defaults to 'f'"
                                              (selection-function sf))

              #:args (filename)
              (list filename (selection-function)))))

        (match-let* (((list filename selection-function) main) ((list code atoms) (translate-file filename selection-function)))
                    (display (string-join (map (lambda (e) (format "thf(~a, type, ( ~a: $i > $o ))." e e)) atoms) "\n"))
                    (newline)
                    (display (format "thf(~a, type, ( ~a: $i > ( $i > $o ) > ( $i > $o ) ))." selection-function selection-function))
                    (newline)
                    (display (string-join (map (lambda (e) (pretty-print-leoii e)) code) "\n"))))
