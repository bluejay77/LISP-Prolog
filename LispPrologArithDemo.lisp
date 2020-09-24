;;;  -*- Mode: Lisp; -*-
;;; Written in 1983 by Ken Kahn and Mats Carlsson.
;;; Modified for the Norvig LISP-PROLOG by AJY 2014-07-23.
;;; Corrected and modified by AJY 2014-07-23.

;;; Peano arithmetic,
;;; factorial and fibonacci originally in LM-Prolog
;;; Converted to the Norvig Prolog by AJY

;;; AJY 2014-07-23
;;; LOAD this file, don't CONSULT it
;;;
;;; Had to fix this quite a lot after the work of Carlsson-Kahn
;;;

;;; Idea: (?- (plus ?sum-of-addends ?addend-1 ?addend-2))
;;; unlike the MRS, where the arguments are the other way round

;;; Try:
;;; (?- (plus ?sum (1+ (1+ (1+ 0))) (1+ (1+ 0))))
;;;
;;; (?- (times ?prod (1+ (1+ 0)) (1+ (1+ 0))))
;;;
;;; (?- (factorial ?fact (1+ (1+ (1+ 0)))))
;;;
;;; (?- (fibonacci ?fi (1+ (1+ (1+ (1+ 0))))))
;;;



;;; Sum:

(<- (plus 0 0 0)) ; Base case
(<- (plus ?sum 0 ?sum)) ; Neutral element
(<- (plus ?sum ?sum 0)) ; Neutral element, commutative
(<- (plus (1+ ?sum) (1+ ?x) . ?addends) ; Recursive case
    (plus ?sum ?x . ?addends))

;;; Times:
(<- (times ?prod (1+ 0) ?prod)) ; neutral element
(<- (times ?prod ?prod (1+ 0)))
(<- (times 0 0 ?mul)) ; null element
(<- (times 0 ?mul 0)) ; null element
(<- (times ?product (1+ ?x-1) . ?multiplicands) ; recursive case
    (times ?product-1 ?x-1 . ?multiplicands)
    (plus ?product ?product-1 . ?multiplicands))

;;; factorial:
(<- (factorial (1+ 0) 0))
(<- (factorial ?factorial (1+ ?n-1))
    (factorial ?factorial-of-n-1 ?n-1)
    (times ?factorial (1+ ?n-1) ?factorial-of-n-1))

;;; fibonacci:
(<- (fibonacci 0 0))
(<- (fibonacci (1+ 0) (1+ 0)))
(<- (fibonacci ?fib (1+ (1+ ?x)))
     (fibonacci ?a (1+ ?x))
     (fibonacci ?b ?x)
     (plus ?fib ?a ?b))

