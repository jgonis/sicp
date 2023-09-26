;;test comment
(define-library (concabs ch2)
  (export ex2-1
          ex2-4
          fast-ex2-1
          fast-iter-ex2-1
          quot
	  subtract-the-first
	  sum-of-squares
	  sum-of-cubes
	  sum-of-powers
	  num-digits
	  number-of-sixes
	  number-of-digit)
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
    (define fast-ex2-1
      (lambda (x y)
        (cond ((= y 0) 1)
              ((even? y) (fast-ex2-1 (* x x) (/ y 2)))
              (else (* x (fast-ex2-1 x (- y 1)))))))
    (define fast-iter-ex2-1
      (lambda (x y)
        (let loop ((base x)
                   (exponent y)
                   (state-var 1))
          (cond ((= exponent 0) 1)
                ((= exponent 1) (* state-var base))
                ((even? exponent) (loop (* base base) (/ exponent 2) state-var))
                (else (loop base (- exponent 1) (* base state-var)))))))

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

    (define quot
      (lambda (n d)
	(cond ((< n 0) (- (quot (- n) d)))
	      ((< d 0) (- (quot n (- d))))
	      ((< n d) 0)
	      (else (+ 1 (quot (- n d) d))))))

    (define remaind
      (lambda (n d)
	(cond ((< n 0) (- (remaind (- n) d)))
	      ())))

    (define subtract-the-first
      (lambda (n)
	(cond ((= n 0) 0)
	      (else (- (subtract-the-first (- n 1)) n )))))

    (define sum-of-squares
      (lambda (n)
	(cond ((= n 0) 0)
	      (else (+ (* n n) (sum-of-squares (- n 1)))))))

    (define sum-of-cubes
      (lambda (n)
	(cond ((= n 0) 0)
	      (else (+ (* n n n) (sum-of-cubes (- n 1)))))))

    (define sum-of-powers
      (lambda (n p)
	(cond ((or (< p 0) (< n 0)) (error "can't have negative arguments"))
	      ((= n 0) 0)
	      (else (+ (fast-iter-ex2-1 n p) (sum-of-powers (- n 1) p))))))

    (define num-digits
      (lambda (n)
	(cond ((< n 0) (num-digits (- n)))
	      ((< n 10) 1)
	      (else (+ 1 (num-digits (quot n 10)))))))
    (define number-of-sixes
      (lambda (n)
	(cond ((< n 0) (number-of-sixes (- n)))
	      ((= (remainder n 10) 6) (+ 1 (number-of-sixes (quot n 10)))))))
    ))




