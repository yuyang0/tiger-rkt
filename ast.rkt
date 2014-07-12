#!/usr/bin/env racket

#lang racket
(require "utils/datatype.rkt")
(define-datatype expression expression?
  [number-const (val number?)]
  [string-const (val string?)]
  [nil]
  [seq-exp (exps (listof expression?))]
  [lval-exp (lval lvalue?)]
  [call-exp (func symbol?)
            (args (listof expression?))]
  [new-array-exp (type symbol?)
                 (max number?)
                 (init expression?)]
  [new-record-exp (listof valfield?)]
  [unary-exp (op unary-op?)
             (exp expression?)]
  [binary-exp (op binary-op?)
              (exp1 expression?)
              (exp2 expression?)]
  [assign-exp (lval lvalue?)
              (val expression?)]
  [if-exp (test expression?)
          (conseq expression?)]
  [ife-exp (test expression?)
           (conseq expression?)
           (alt expression?)]
  [while-exp (test expression?)
             (body expression?)]
  [break-exp]
  [for-exp (var symbol?)
           (down expression?)
           (up expression?)
           (body expression?)]
  [let-exp (decls (listof declaration?))
           (body (listof expression?))])

(define-datatype valfield valfield?
  [a-valfield (name symbol?)
           (val expression?)])
(define-datatype tyfield tyfield?
  [a-tyfield (name symbol?)
           (type-id symbol?)])

(define-datatype declaration declaration?
  [type-decl (type-id symbol?)
             (type-val type-expression?)]
  [value-decl (name symbol?)
              (val expression?)]
  [value-ty-decl (name symbol?)
                 (ty symbol?)
                 (val expression?)]
  [function-decl (name symbol?)
                 (args (listof tyfield))
                 (body expression?)]
  [function-ty-decl (name symbol?)
                    (args (listof tyfield))
                    (ret-ry symbol?)
                    (body expression?)])

(define-datatype lvalue lvalue?
  [id-lval (name symbol?)]
  [sub-lval (name symbol?)
            (idx-lst (listof expression?))]
  [attr-lval (lval lvalue?)
             (name symbol?)])

(define unary-op?
  (lambda (op)
    (memv op '(-))))

(define binary-op?
  (lambda (op)
    (memv op '(+ - * / > >= < <= = and or))))
