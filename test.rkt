#!/usr/bin/env racket

#lang racket
(define test
  (lambda (dir test-func)
    (let ([file-lst (filter file-exists? (directory-list dir #:build? #t))])
      (map test-func file-lst))))
(define test-parser
  (lambda ()
    (test "testcases" gen-ast)))
