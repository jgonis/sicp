;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions
;; (define (load-func)
;;   (load "/home/jgonis/code/sicp/sicp/ch1/ch1.scm")
;;   (load "/home/jgonis/code/sicp/sicp/ch2/ch2.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/fp-compare.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/helpers.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch1/ch1Tests.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch2/ch2Tests.scm"))

(define-library (ch2 ch2)
  (export make-rational
	  add-rational
	  sub-rational
	  mult-rational
	  div-rational
	  equal-rational)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (scheme inexact)
	  (srfi 1)
	  (srfi 27)
          (libs fp-compare)
	  (libs helpers))
  (begin
    (define (test-func) 1)
    (define (make-rational numerator denominator) 1)
    (define (j-numerator rational) 1)
    (define (j-denominator rational) 1)
    (define (add-rational x y)
      (make-rational (+ (* (j-numerator x) (j-denominator y))
			(* (j-numerator y) (j-denominator x)))
		     (* (j-denominator x) (j-denominator y))))
    (define (sub-rational x y)
      (make-rational (- (* (j-numerator x) (j-denominator y))
			(* (j-numerator y) (j-denominator x)))
		     (* (j-denominator x) (j-denominator y))))
    (define (mult-rational x y)
      (make-rational (* (j-numerator x) (j-numerator y))
		     (* (j-denominator x) (j-denominator y))))
    (define (div-rational x y)
      (make-rational (* (j-numerator x) (j-denominator y))
		     (* (j-denominator x) (j-numerator y))))
    (define (equal-rational x y)
      (and (eqv? (j-numerator x) (j-numerator y))
	   (eqv? (j-denominator x) (j-denominator y))))))
