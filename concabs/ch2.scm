;;test comment
(define-library (concabs ch2)
  (export ex2-1
          ex2-4)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (scheme inexact)
	  (srfi 1)
	  (srfi 27)
	  (concabs helpers)
	  (concabs fungraph-svg)
	  (concabs quilting))
  (begin    
    (define (ex2-1 x y)
      (cond ((= y 0) 1)
	    (else (* x (ex2-1 x (- y 1))))))

    (define candy-temperature
      (lambda (temp elevation)
	(let ((multiplier (quotient elevation 500)))
	  (- temp multiplier))))
    
    (define tax
      (lambda (income)
	(cond ((< income 10000) 0)
	      (else (* 0.2 (- income 10000))))))

    ;; Add a to the larger of b or c
    (define puzzle1
      (lambda (a b c)
	(+ a (if (> b c)
		 b
		 c))))
    ;; If an argument passed in is negative turn it positive
    ;; basically an absolute value function
    (define puzzle2
      (lambda (x)
	((if (< x 0)
	     -
	     +)
	 0 x)))
    
    (define ex2-4
      (lambda (n)
        (cond ((= n 0) 0)
              ((even? n) (* (ex24 (/ n 2)) 4))
              (else (+ (ex24 (- n 1)) (- (+ n n) 1))))))
    ))




