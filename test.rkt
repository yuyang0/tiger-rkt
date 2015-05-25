#!/usr/bin/env racket

#lang racket
(require "parser.rkt")
(define test
  (lambda (dir test-func)
    (let ([file-lst (filter file-exists? (directory-list dir #:build? #t))])
      (let loop ([lst file-lst] [result '()])
        (if (null? lst)
            (begin
              (printf "-------------------------------------\n")

              (for-each
               (lambda (v)
                 (let ([fname (car v)]
                       [ret (cadr v)])
                  (when (not (eq? ret #t))
                        (printf "~a ~a \n" fname ret))))
               result))
            (begin
              (printf "Parsing ~a ...\n" (car lst))
              (let  ([ret (with-handlers ([string? (lambda (v) v)])
                                         (test-func (car lst))
                                         #t)])
                (loop (cdr lst) (cons `(,(car lst) ,ret) result)))))))))
(define test-parser
  (lambda ()
    (test "testcases" gen-ast)))
(test-parser)
