(module macl2leoii scheme
		(provide translate-file pretty-print-leoii)

		(require parser-tools/yacc
				 parser-tools/lex
				 (prefix-in : parser-tools/lex-sre)
				 syntax/readerr
				 scheme/string
				 scheme/cmdline
				 scheme/match)

		(define-tokens value-tokens (ATOM))
		(define-empty-tokens op-tokens (EOF LPAREN RPAREN NOT AMPERAMPER BARBAR EQUALSGREATER DOT))

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
			("=>" 'EQUALSGREATER)
			("." 'DOT)
			((:: (:+ lower-letter) (:* (:or lower-letter upper-letter "_" digit))) (token-ATOM (string->symbol lexeme)))))

		(define dslp
		  (lambda (source-name f initial-world)
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

			  (precs (left EQUALSGREATER)
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
					  ((expr EQUALSGREATER expr) (let ((x (gensym "X"))
													   (w (gensym "W")))
												   `(lambda ,x (forall ,w (implies (app (app ,f ,initial-world ,$1) ,x ,w) (app ,$3 ,w)))))))
				(expr-list ((expr-list DOT expr) (cons `(formula ,$3) $1))
						   ((expr) (list `(formula ,$1)))
						   ((expr-list DOT) $1))
				(simple-expr ((ATOM) $1)
							 ((LPAREN expr RPAREN) $2))))))

		(define translate
		  (lambda (s f initial-world #:src-name (src-name "current-input-port"))
			(let ((ois (open-input-string s)) (statements '()))
			  (port-count-lines! ois)
			  ((dslp src-name f initial-world) (lambda () (dsll ois))))))

		(define translate-file
		  (lambda (path f initial-world)
			(call-with-input-file path
								  (lambda (in)
									(translate (port->string in) f initial-world #:src-name path)))))

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
				  ((formula) (format "thf(f~a, ROLE???, ( ~a ))." (counter) (pretty-print-leoii (list-ref code 1))))
				  ((implies) (format "( ~a => ~a )" (pretty-print-leoii (list-ref code 1)) (pretty-print-leoii (list-ref code 2))))
				  ((lambda) (format "( ^ [~a: $i] : ~a )" (list-ref code 1) (pretty-print-leoii (list-ref code 2))))
				  ((not) (format "~~ ~a" (pretty-print-leoii (list-ref code 1))))
				  ((or) (format "( ~a | ~a )" (pretty-print-leoii (list-ref code 1)) (pretty-print-leoii (list-ref code 2)))))))))

		(define main
		  (let ((f (make-parameter 'f)) (initial-world (make-parameter 't)))
			(command-line
			  #:once-each
			  (("-f" "--function ") if
									"Function: defaults to 'f'"
									(f if))
			  (("-i" "--initial-world") i
										"Initial world: defaults to 't'"
										(initial-world i))

			  #:args (filename)
			  (list filename (f) (initial-world)))))

		(match-let* (((list filename f initial-world) main))
					(let ((code (translate-file filename f initial-world)))
					  (display (string-join (map (lambda (e) (pretty-print-leoii e)) code) "\n")))))
