(define-library (concabs ch3)
  (export fib
	  iter-fib
	  fermat-number
	  is-prime?)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (scheme inexact)
	  (srfi 1)
	  (srfi 27)
	  (concabs helpers)
	  (concabs fungraph-svg)
	  (concabs quilting)
	  (concabs digits))
  (begin
    (define fib
      (lambda (n)
	(cond ((= n 0) 0)
	      ((= n 1) 1)
	      (else (+ (fib (- n 1)) (fib (- n 2)))))))
    (define iter-fib
      (lambda (n)
	(cond ((= n 0) 0)
	      ((= n 1) 1)
	      (else (let loop ((a 0)
			       (b 1)
			       (i 2))
		      (cond ((= i n) (+ a b))
			    (else (loop b (+ a b) (+ i 1)))))))))
    (define fermat-number
      (lambda (n)
	(+ 1  (let loop ((a 2)
			 (b n))
		(cond ((= b 0) a)
		      (else (loop (* a a) (- b 1))))))))
    (define is-prime?
      (lambda (n)
	#t))))
