(define-library (concabs ch4)
  (export verify
	  mod+
	  mod-)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (scheme inexact)
	  (srfi 1)
	  (srfi 27)
	  (concabs helpers))
  (begin
    (define verify
      (lambda (signature modulus)
	(remainder (expt signature 3)
		   modulus)))

    (define mod+
      (lambda (x y modulus)
	(remainder (+ x y) modulus)))

    (define mod-
      (lambda (x y modulus)
	(mod+ x (- modulus y) modulus)))))
