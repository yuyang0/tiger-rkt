#!/usr/bin/env racket

#lang racket
(require "datatype.rkt")
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (num-list
   (value (listof number?)))
  (empty))

(cases expval (num-list '(1 3 4))
       [num-val (val)
                val]
       [bool-val (val)
                 val]
       [num-list (val)
                 val]
       [empty '()])
(cases expval (empty)
       [num-val (val)
                val]
       [bool-val (val)
                 val]
       [num-list (val)
                 val]
       [empty '()])
