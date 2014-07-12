#!/usr/bin/env racket

#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre)) ;overwrite or, so give a prefix

;;---------------------------------------------------------------
;;    Tokens
;;---------------------------------------------------------------
(define-empty-tokens keywords
  (WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN ELSE DO
         OF NIL))

;; because ( ) [ ] { } , ; . are delimeters, so we must use | to get
;; the symbol
(define-empty-tokens delims
  (|(| |)| |[| |]| |{| |}| |,| |;| |.| : EOF))

(define-empty-tokens operators
  (MINUS + - * / = <> < <= > >= := &  \|))

(define-tokens constants
  (NUMBER STRING))

(define-tokens identifier
  (ID))

;;; ignore comments(support nested comments)
;;; comment form: /* ... */
(define comment
  (lambda (port)
    (let loop ([prev (read-char port)]
               [cur (read-char port)])
      (if (and (char? prev)
               (char? cur)) ;;(eof-object? prev)
          (let ([ss (string prev cur)])
            (cond
             [(string=? ss "*/") #t]
             [(string=? ss "/*") (comment port) (loop cur (read-char port))]
             [else (loop cur (read-char port))]))
          (error 'comment "unclosed comment")
          ))))

(define-lex-abbrevs
  [lower-letter (:/ "a" "z")]
  [upper-letter (:/ #\A #\Z)]
  [letter (:or lower-letter upper-letter)]
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.
  ;; (:/ #\0 #\9) is ok too.
  [digit (:/ "0" "9")]
  [hex-digit (:or digit (:/ "a" "f") (:/ "A" "F"))]
  [letter-digit (:or digit letter)]
  [sign (:or "+" "-")]
  [hex-number (:seq (:? sign) (:or "0x" "0X") (:* hex-digit))]
  [oct-number (:seq (:? sign) "0" (:* (:/ "0" "7")))])

(define next-token
  (lexer-src-pos
   [(eof) 'EOF]
   ;; whitespaces
   [(char-set " \t\v\f\r\n")
    (return-without-pos (next-token input-port))]
   ;; block comments
   ["/*"
    (begin (comment input-port)
           (return-without-pos (next-token input-port)))]
   ;; line comments
   [(:seq "//" (:* (:~ "\n")))
    (return-without-pos (next-token input-port))]
   ;; keywords
   [(:or "while" "for" "to" "break" "let" "in" "end" "function"
         "var" "type" "array" "if" "then" "else" "do" "of" "nil")
    (string->symbol (string-upcase lexeme))]
   ;; delimeters
   [(:or "(" ")" "[" "]" "{" "}" "," ";" "." ":")
    (string->symbol lexeme)]
   ;; operators
   [(:or "+" "-" "*" "/" "="
         "<>" ">" ">=" "<" "<=" ":=" "&" "|")
    (string->symbol lexeme)]

   [(:seq (:or letter "_") (:* (:or letter-digit "_")))
    (token-ID (string->symbol lexeme))]
   ;; [+-]?[0-9]+
   [(:seq (:+ digit))
    (token-NUMBER (string->number lexeme))]
   ;; string literal \"(\\.|[^\\"\n])\"
   [(:seq "\"" (:* (:or (:seq "\\" any-char) (:~ "\n" "\\" "\""))) "\"")
    (token-STRING lexeme)]))
(define gen-tokens
  (lambda (fname)
    (let ([port (open-input-file fname)])
      (port-count-lines! port)

      (let loop ([tok (next-token port)])
        (let ([real-tok (if (position-token? tok)
                            (position-token-token tok)
                            tok)])
          (if (eq? real-tok 'EOF)
              (display real-tok)
              (begin
                (printf "~a " real-tok)
                (loop (next-token port)))))))))
;;-------------------------------------------------------------
;;       parser
;;-------------------------------------------------------------
(require parser-tools/yacc)

(struct Node (type
              ;; start
              ;; end
              elts)
        #:transparent)
(define tiger-parser
  (parser
   [src-pos]

   [debug "/home/yangyu/tmp/debug.rkt"]

   [start program]
   [end EOF]
   [tokens keywords constants operators identifier delims]
   [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error 'tiger-parser "error (~a:~a) token-name: ~a token-value: ~a"
                   (position-line start-pos) (position-col start-pos)
                   tok-name tok-value))]
   [precs [left & \|]
          [nonassoc = <> < <= > >=]
          [left + -]
          [left * /]
          [right MINUS]]
   [grammar
    (program [(exp) $1])

    (type-id [(ID) (Node 'name $1)])
    (identifier  [(ID) (Node 'name $1)])

    (tyfield [(identifier : type-id) `(,$1 ,$3)])
    (tyfield-list [(tyfield-list |,| tyfield) (append $1 `(,$3))]
                  [(tyfield) `(,$1)])
    (tyfields [(tyfield-list) (Node 'tyfields $1)]
              [() '(Node 'tyfields '())])

    (type-exp
     [(type-id) $1]
     [(|{| tyfields |}|) (Node 'record-type $2)]
     [(ARRAY OF type-id) (Node 'array-type $3)])
    (type-decl [(TYPE type-id = type-exp) (Node 'type-decl `(,$2 ,$4))])
    (var-decl [(VAR identifier := exp) (Node 'var-decl `(,$2 ,$4))]
              [(VAR identifier : type-id := exp)
               (Node 'var-decl `(,$2 ,$4 ,$6))])
    (func-decl [(FUNCTION identifier |(| tyfields |)| = exp)
                (Node 'func-decl `(,$2 ,$4 ,$7))]
               [(FUNCTION identifier |(| tyfields |)| : type-id = exp)
                (Node 'func-decl `(,$2 ,$4 ,$7 ,$9))])
    (decl
     [(type-decl) $1]
     [(var-decl) $1]
     [(func-decl) $1])
    (decls [(decl decls) (cons $1 $2)]
           [() '()])
    (array [(|[| exp |]|) `(,$2)]
           [(|[| exp |]| array) (cons $2 $4)])
    (lvalue [(identifier) $1]
            [(lvalue |.| identifier) (Node 'attribute `(,$1 ,$3))]
            [(identifier array) (Node 'subscript `(,$1 ,$2))])
    (exp-list [(exp) `(,$1)]
              [(exp-list |;| exp) (append $1 `(,$3))])
    (exps [(exp-list) $1]
          [() '()])
    (arg-list [(exp) `(,$1)]
              [(arg-list |,| exp) (append $1 `(,$3))])
    (args [(arg-list) $1]
          [() '()])

    (valfield [(identifier = exp) `(,$1 ,$3)])
    (valfield-list [(valfield-list |,| valfield) (append $1 `(,$3))]
                   [(valfield) $1])
    (valfields [(valfield-list) (Node 'valfields $1)]
               [() '(Node 'valfields '())])

    (exp
     [(NUMBER) (Node 'number-literal $1)]
     [(STRING) (Node 'string-literal $1)]
     [(lvalue) $1]
     [(NIL) (Node 'nil '())]
     [(|(| exps |)|) (Node 'exp-seq $2)]
     [(identifier |[| exp |]| OF exp) (Node 'array-exp `(,$1 ,$3 ,$6))]
     [(identifier |{| identifier = exp |}|)
      (Node 'record-exp `(,$1 ,$3 ,$5))]
     [(identifier |(| args |)|) (Node 'call-exp `(,$1 ,$3))]

     [(unary-exp) $1]

     [(arith-exp) $1]
     [(rel-exp) $1]
     [(logic-exp) $1]

     [(lvalue := exp) (Node 'assign-exp `(,$1 ,$3))]
     [(IF exp THEN exp) (Node 'if-exp `(,$2 ,$4))]
     [(IF exp THEN exp ELSE exp) (Node 'ife-exp  `(,$2 ,$4 ,$6))]
     [(FOR identifier := exp TO exp DO exp)
      (Node 'for-exp `(,$2 ,$4 ,$6 ,$8))]
     [(WHILE exp DO exp) (Node 'while-exp `(,$2 ,$4))]
     [(BREAK) (Node 'break-exp '())]
     [(LET decls IN exps END) (Node 'let-exp `(,$2 ,$4))])

    (unary-exp [(- exp) (Node 'minus $2)])
    (arith-exp
     [(exp + exp) (Node '+ `(,$1 ,$3))]
     [(exp - exp) (Node '- `(,$1 ,$3))]
     [(exp * exp) (Node '* `(,$1 ,$3))]
     [(exp / exp) (Node '/ `(,$1 ,$3))])
    (rel-exp
     [(exp > exp) (Node '> `(,$1 ,$3))]
     [(exp >= exp) (Node '>= `(,$1 ,$3))]
     [(exp < exp) (Node '< `(,$1 ,$3))]
     [(exp <= exp) (Node '<= `(,$1 ,$3))]
     [(exp = exp) (Node '= `(,$1 ,$3))]
     [(exp <> exp) (Node '<> `(,$1 ,$3))])
    (logic-exp
     [(exp & exp) (Node 'and `(,$1 ,$3))]
     [(exp \| exp) (Node 'or `(,$1 ,$3))])
    ]))

(define gen-ast
  (lambda (fname)
    (let ([port (open-input-file fname)])
      (port-count-lines! port)
      (tiger-parser (lambda ()
                      (next-token port))))))

(module+ main
         ;;(gen-tokens "tests/test-1.tig")
         (display (gen-ast "tests/test-1.tig"))
         ;;(display (gen-ast "tests/sample.tig"))

         )
