#!/usr/bin/env racket

#lang racket
(provide ext lookup)

(define ext
  (lambda (var val e)
    (cons `(,var ,val) e)))
(define lookup
  (lambda (var e)
    (cond
     [(assq var e) => cadr]
     [else #f])))

(define  (empty-env) '())
(define (init-type-env)
  '((string string) (int int)))
