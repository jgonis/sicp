(define-library (concabs ch3)
  (export fib
	  iter-fib
	  fermat-number
	  is-prime?
	  perfect-number?
	  first-perfect-after
	  sum-of-divisors
	  find-approximation-from)
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
	(if (= 0 (remainder n 2))
	    #f
	    (let loop ((i 3))
	      (cond ((> (* i i) n) #t)
		    ((= 0 (remainder n i)) #f)
		    (else (loop (+ i 2))))))))
    
    (define sum-of-divisors
      (lambda (n)
	(define sum-from-plus
	  (lambda (current addend)
	    (cond ((> (* current current) n) addend)
		  ((= 0 (remainder n current))
		   (let ((quot (/ n current)))
		     (cond ((= quot current) (sum-from-plus (+ current 1)
							    (+ addend current)))
			   (else (sum-from-plus (+ current 1)
						(+ addend current quot))))))
		  (else (sum-from-plus (+ current 1) addend)))))
	(sum-from-plus 1 0)))

    (define perfect-number?
      (lambda (n) 
	(= (* 2 n) (sum-of-divisors n))))

    (define first-perfect-after
      (lambda (n)
	(cond ((perfect-number? (+ n 1)) (+ n 1))
	      (else (first-perfect-after (+ n 1))))))
    (define find-approximation-from
      (lambda (starting-point)
	(cond ((good-enough? starting-point) starting-point)
	      (else (find-approximation-from (improve starting-point))))))))
