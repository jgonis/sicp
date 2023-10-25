(define-library (concabs ch3)
  (export fib
	  iter-fib
	  fermat-number
	  is-prime?
	  perfect-number?
	  first-perfect-after
	  sum-of-divisors
	  find-approximation-from
	  approximate-square-root
	  approximate-golden-ratio
	  survives?
	  first-survivor-after
	  falling-factorial-power
	  jexpt
	  verify)
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
      (lambda (starting-point good-enough? improve)
	(cond ((good-enough? starting-point) starting-point)
	      (else (find-approximation-from (improve starting-point) good-enough? improve)))))
    
    (define approximate-square-root
      (lambda (number)
	(define good-enough?
	  (lambda (guess)
	    (let ((tolerance 1/100000000))
	      (< (abs (- guess number)) tolerance))))
	(let loop ((low 0)
		   (high number))
	  (let* ((guess (+ (/ (- high low) 2) low))
		 (result (* guess guess)))
	    (cond ((good-enough? result) guess)
		  ((< result number) (loop guess high))
		  (else (loop low guess)))))))
    
    (define approximate-golden-ratio
      (lambda (tolerance)
	(define improve-guess
	  (lambda (previous-guess)
	    (+ 1 (/ 1 previous-guess))))
	(define good-enough?
	  (lambda (current-guess)
	    (< (/ 1 (square (denominator current-guess))) tolerance)))
	(find-approximation-from 1 good-enough? improve-guess)))

    (define survives?
      (lambda (position number-of-people)
	(cond ((< number-of-people 3) #t)
	      ((= position 3) #f)
	      ((> position 3) (survives? (- position 3) (- number-of-people 1)))
	      (else (survives? (- number-of-people (- 3 position)) (- number-of-people 1))))))
    
    (define first-survivor-after
      (lambda (starting-point circle-size)
	(cond ((> starting-point circle-size) -1)
	      ((survives? (+ starting-point 1) circle-size) (+ starting-point 1))
	      (else (first-survivor-after (+ starting-point 1) circle-size)))))

    (define falling-factorial-power
      (lambda (n k)
	(define helper
	  (lambda (n k total)
	    (cond ((= k 0) total)
		  (else (helper (- n 1) (- k 1) (* total n))))))
	(helper n k 1)))

    (define jexpt
      (lambda (b n)
	(define helper
	  (lambda (n total)
	    (cond ((= n 0) total)
		  ((< n 0) (helper (+ n 1) (/ total b)))
		  ((> n 0) (helper (- n 1) (* total b))))))
	(helper n 1)))

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
