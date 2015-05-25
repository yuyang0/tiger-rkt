#!/usr/bin/env racket

#lang racket
(require "types.rkt")


(define (check-exp exp env tenv)
  (cases expression exp
         [number-const-exp (val) 'int]
         [string-const-exp (val) 'string]
         ;; [name-exp (name)
         ;;           (let ([ty (lookup name env)])
         ;;             (if ty
         ;;                 ty
         ;;                 (error "identifier is not ")))]
         [nil-exp 'nil]
         [seq-exp (exps)
                  (if (null? exps)
                      'nil
                      (last (map (lambda (exp) (check-exp exp env tenv))
                                 exps)))]
         [lval-exp (lval) (check-lval lval env tenv)]
         [call-exp (func args)]
         [new-array-exp (type max init)]
         [new-record-exp ()]
         [new-array-exp ()]
         [unary-exp (op exp)
                    (let ([ty (check-exp exp env tenv)])
                      (cond
                       [(eq? ty 'int)
                        (if (eq? op '-)
                            'int
                            (error "argument for minus must be integers"))]
                       [else (error "unkown unary operator")]))]
         [binary-exp (op exp1 exp2)
                     (let ([ty1 (check-exp exp1 env tenv)]
                           [ty2 (check-exp exp2 env tenv)])
                       (cond
                        [(memq op '(+ - * /))
                         (if (and (eq? ty1 'int) (eq? ty2 'int))
                             'int
                             (error "the argument for arithmetic operators must be numbers"))]
                        [(memq op '(= <> > >= < <=))
                         (cond
                          [(and (int-type? ty1) (int-type? ty2))
                           'int]
                          [(and (string-type? ty1) (string-type? ty2))
                           'int]
                          [(and (memq op '(= <>)) (type-equal? ty1 ty2))
                           'int]
                          [else (error "wrong argment for relational operator")])]
                        [(memq op '(and or)) 'int]
                        [else (error "unkown unary operator")]))]
         [assign-exp (lval val)
                     (let ([ty1 (check-lval lval env tenv)]
                           [ty2 (check-exp val env tenv)])
                       (if (type-equal? ty1 ty2)
                           'nil
                           (error "assign-exp: types are inconsisitence")))]
         [if-exp (exp1 exp2)
                 (let ([ty1 (check-exp exp1 env tenv)]
                       [ty2 (check-exp exp2 env tenv)])
                    (cond
                      [(and (eq? ty1 'int)(eq? ty2 'nil)) 'nil]
                      [else (error "test for if is not an integer expression")]))]
         [ife-exp (exp1 exp2 exp3)
                   (let ([ty1 (check-exp exp1 env tenv)]
                         [ty2 (check-exp exp2 env tenv)]
                         [ty3 (check-exp exp3 env tenv)])
                    (cond
                      [(and (eq? ty1 'int) (type-equal ty2 ty3)) ty2]
                      [else (error "test for if is not an integer expression")]))]
         [while-exp (test body)
                    (let ([ty1 (check-exp test env tenv)]
                          [ty2 (check-exp body env tenv)])
                      (cond
                       [(and (eq? ty1 'int) (eq? ty2 'nil)) 'nil]
                       [else (error "test of while exp must be integer exp")]))]
         [for-exp (var down up body)
                  (let ([down-ty (check-exp down env tenv)]
                        [up-ty (check-exp up env tenv)]
                        [body-ty (check-exp body (ext var 'int tenv) tenv)])
                    (cond
                     [(and (eq? down-ty 'int)(eq? up-ty 'int) (eq? body-ty 'nil)) 'nil]
                     [else (error "the bound exp of for must be integer")]))]
         [let-exp (decls body)
                  (let-values ([(new-env new-tenv) (check-decls decls env tenv)])
                    (check-exp body new-env new-tenv))]))

(define (check-decls decls env tenv)
  (define (check-type-exp exp env tenv)
    (case type-expression exp
          [name-tyexp (name)
                      (let ([ty (lookup name tenv)])
                        (if ty
                            ty
                            (error "type variable is not bound")))]
          [array-tyexp (elt-ty)
                       (let ())]))
  (define (check-decl decl env tenv)
    (cases declaration decl
           [type-decl (type-id type-val)
                      ]
           [value-decl (name val)
                       (let ([ty (check-exp val env tenv)])
                         (values (ext name ty env) tenv))]
           [value-ty-decl (name ty val)
                          (let ([ty1 (check-exp val env tenv)])
                            (if (type-equal? ty ty1)
                                (values (ext name ty env) tenv)
                                (error "type of value declaration is inconsistent")))]
           [function-decl (name args body)
                          ]
           [function-ty-decl (name args ret-ty body)
                             ]))
  (define (check-type-decls decls env tenv) ;support mutual recursion
    )
  (define (check-function-decl decls env tenv) ;support mutual recursion
    )
  (let loop ([env env]
             [tenv tenv]
             [decls decls])
    (if (null? decls)
        (values env tenv)
        (let-values ([(new-env new-tenv)
                      (check-decl (car decls) env tenv)])
          (loop new-env new-tenv (cdr decls))))))

(define check-lval
  (lambda (exp env tenv)
    (cases lvalue exp
           [id-lval (name)
                   (let ([ty (lookup name env)])
                     (if ty
                         ty
                         (error "identifier is not ")))]
           [sub-lval (lval idx)
                     (let ([ty1 (check-exp lval env tenv)]
                           [ty2 (check-exp idx env tenv)])
                       (if (type-equal? ty2 'int)
                           ty1
                           (error "index of subscript is not interger")))]
           [attr-lval (lval name)       ;TODO:
                      (let ([ty1 (check-exp lval env tenv)]
                           [ty2 (check-exp idx env tenv)])
                       (if (record-type? ty1)
                           ty1
                           (error "index of subscript is not interger")))])))
