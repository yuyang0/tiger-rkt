#!/usr/bin/env racket

#lang racket

(provide (all-defined-out))
;; -------------- basic types --------------------------
;; int
;; string
;; nil
(define int-type?
  (lambda (ty) (eq? ty 'int )))
(define string-type?
  (lambda (ty) (eq? ty 'string )))
(define nil-type?
  (lambda (ty) (eq? ty 'nil )))
;;; int can be used as a bool type
;; (define bool-type?
;;   (lambda (ty) (memq ty '(bool int))))

(define foldl2
  (lambda (f x ls)
    (cond
     [(null? ls) x]
     [else
      (foldl2 f (f x (car ls)) (cdr ls))])))

(struct fun-type (args ret) #:transparent)
(define print-fun-type
  (lambda (ty)
    (let ([args (fun-type-args ty)]
          [ret (fun-type-ret ty)])
      `(,(foldl2 (lambda (x y)
                   (if (null? x)
                       `(,y)
                       (append x `(* ,y))))
              '()
              args)
        -> ,ret))))

;; (print-fun-type (fun-type '(int string int) 'string))
;; => '((int * string * int) -> string)

;; record type
;; array type
(struct array-type (elt)
        #:extra-constructor-name listof
        #:transparent)
(define print-array-type
  (lambda (ty)
    `(listof ,(array-type-elt ty))))
;; (print-array-type (array-type 'int))
;; (print-array-type (listof 'int))

;; record type
(struct record-type (names types)
        #:transparent)
(define print-record-type
  (lambda (ty)
    ))

(define type-equal? equal?)
