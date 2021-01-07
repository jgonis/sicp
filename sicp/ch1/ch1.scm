;;java -jar kawa.jar -s -r7rs
;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions
;; (define (load-func)
;;   (load "/home/jgonis/code/sicp/sicp/ch1/ch1.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/fp-compare.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch1/ch1Tests.scm"))

(define-library (ch1 ch1)
  (export ex1-2
          ex1-3
          j-abs
          j-sqrt
          alt-j-sqrt
          ex1-8
	  Ackermann
	  jfib
	  jfib-iter
	  count-change-combinations
	  ex1-11
	  ex1-11-iter
	  pascals-triangle
	  j-expt
	  j-expt-iter
	  fast-expt
	  ex1-16
	  ex1-17
	  ex1-18
	  prime?
	  fast-prime?)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (srfi 1)
	  (srfi 27)
          (libs fp-compare))
  (begin    
    (define (sum-of-squares x y)
      (+ (square x) (square y)))

    (define (ex1-2)
      (/ (+ 5
            4
            (- 2
               (- 3
                  (+ 6 4/5))))
         (* 3
            (- 6 2)
            (- 2 7))))
    
    (define (ex1-3 x y z)
      (cond ((and (<= x y) (< x z)) (sum-of-squares y z))
            ((and (< y z) (<= y x)) (sum-of-squares x z))
            (else (sum-of-squares x y))))
    
    (define (j-abs x)
      (cond ((< x 0) (- x))
            (else x)))

    (define (j-sqrt x)
      (define (good-enough-direct-compare? guess x)
        (fp-eq? (square guess) x))
      (j-square-root x good-enough-direct-compare?))
    
    (define (alt-j-sqrt x)
      (j-square-root x good-enough-guess-difference?))

    (define (improve-guess guess x)
      (define (j-average x y)
        (/ (+ x y)
           2))
      (j-average guess (/ x guess)))
    
    ;;Exercise 1.7
    (define good-enough-guess-difference?
      (case-lambda
       ((guess x)
	(good-enough-guess-difference? guess x improve-guess))
       ((guess x improve-func)
	(let* ((new-guess (improve-func guess x))
	       (guess-diff (abs (- new-guess guess)))
	       (guess-fraction (/ guess 100000000.0)))
	  (<= guess-diff guess-fraction)))))
    
    (define (j-square-root x good-enough-func?)
      (define (sqrt-iter x guess iterations)
        (cond ((good-enough-func? guess x) guess)
	      (else (sqrt-iter x
			       (improve-guess guess x)
			       (+ iterations 1)))))
      (sqrt-iter x 1.0 1))
    
    (define (ex1-6)
      (let ((a "the new-if fails if passed functions as then or else-clauses because it")
            (b "tries to evaluate them as part of applicative-order evaluations. So the")
            (c "function goes into an infinite loop of calling itself"))))

    (define (ex1-8 x)
      (define (cube-root-iter x guess)
        (cond ((good-enough-guess-difference? guess
					      x
					      improve-guess-cube-root)
	       guess)
	      (else (cube-root-iter x
				    (improve-guess-cube-root guess
							     x)))))
      (define (improve-guess-cube-root guess x)
        (/ (+ (/ x (square guess))
	      (* 2 guess))
           3))
      (cube-root-iter x 1.0))
    
    (define (jfactorial n)
      (cond ((= n 1) 1)
	    (else (* n (jfactorial (- n 1))))))
    
    (define (jfactorial-iterative n)
      (define (jfactorial-helper count product)
	(cond ((> count n) product)
	      (else (jfactorial-helper (+ count 1) (* product count)))))
      (jfactorial-helper 1 1))

    (define (ex1-9)
      ;; The first method define a recursive process as its deferred
      ;; inc operations will grow with each step of the procedure.
      ;; 
      ;; The second method is iterative as it does not require any
      ;; deferred operations and its state is captured in the a and b
      ;; variables.
      )
    
    (define (Ackermann x y)
      (cond ((= y 0) 0)
	    ((= x 0) (* 2 y))
	    ((= y 1) 2)
	    (else (Ackermann (- x 1)
			     (Ackermann x (- y 1))))))
    ;; (Ackermann 1 10) -> 1024
    ;; (Ackermann 2 4) -> 65536
    ;; (Ackermann 3 3) -> 65536
    (define (f n) (Ackermann 0 n)) ;; Multiplies 2 by n
    (define (g n) (Ackermann 1 n)) ;; Raises 2 to n.
    (define (h n) (Ackermann 2 n)) ;; Raises 2 to (h (- n 1))
    (define (k n) (* 5 n n))	   ;; Multiplies square of n by 5.

    (define (jfib n)
      (cond ((= n 0) 0)
	    ((= n 1) 1)
	    (else (+ (jfib (- n 1))
		     (jfib (- n 2))))))
    (define (jfib-iter n)
      (define (helper fib-prev fib-current count)
	(cond ((= count n) fib-current)
	      (else (helper fib-current
			    (+ fib-current fib-prev)
			    (+ count 1)))))
      (cond ((= n 0) 0)
	    ((= n 1) 1)
	    (else (helper 0 1 1))))

    (define (count-change-combinations amount)
      (define (cc amount number-of-different-coins)
	(cond ((= amount 0) 1)
	      ((or (< amount 0)
		   (= 0 number-of-different-coins)) 0)
	      (else (+ (cc amount
			   (- number-of-different-coins 1))
		       (cc (- amount
			      (coin-value number-of-different-coins))
			   number-of-different-coins)))))
      (define (coin-value number-of-coin-denominations)
	(cond ((= number-of-coin-denominations 1) 1)
	      ((= number-of-coin-denominations 2) 5)
	      ((= number-of-coin-denominations 3) 10)
	      ((= number-of-coin-denominations 4) 25)
	      ((= number-of-coin-denominations 5) 100)
	      ((= number-of-coin-denominations 6) 200)
	      (else (error "unknown coin denomination"
			   number-of-coin-denominations))))
      (cc amount 6))

    (define (ex1-11 n)
      (cond ((< n 3) n)
	    (else (+ (ex1-11 (- n 1))
		     (* 2 (ex1-11 (- n 2)))
		     (* 3 (ex1-11 (- n 3)))))))
    
    (define (ex1-11-iter n)
      (define (helper n-3 n-2 n-1 count)
	(cond ((= count n) n-1)
	      (else (helper n-2
			    n-1
			    (+ n-1 (* 2 n-2) (* 3 n-3))
			    (+ count 1)))))
      (cond ((< n 3) n)
	    (else (helper 0 1 2 2))))

    (define (pascals-triangle row column)
      (cond ((and (= row 1) (= column 1)) 1)
	    ((= 0 column) 0)
	    ((> column row) 0)
	    (else (+ (pascals-triangle (- row 1) (- column 1))
		     (pascals-triangle (- row 1) column)))))

    (define (j-expt b n)
      (cond ((= n 0) 1)
	    (else (* b (j-expt b (- n 1))))))

    (define (j-expt-iter b n)
      (define (helper-iter b counter product)
	(cond ((= counter n) product)
	      (else (helper-iter b (+ counter 1) (* b product)))))
      (helper-iter b 0 1))

    (define (fast-expt b n)
      (cond ((= n 0) 1)
	    ((odd? n) (* b (fast-expt b (- n 1))))
	    (else (square (fast-expt b (/ n 2))))))

    (define (ex1-16 b n)
      (define (helper-iter b counter product)
	(cond ((= counter 0) product)
	      ((odd? counter) (helper-iter b (- counter 1) (* product b)))
	      (else (helper-iter (square b) (/ counter 2) product))))
      (helper-iter b n 1))

    (define (fast-expt-iter b n)
      (ex1-16 b n))

    (define (ex1-17 a b)
      (define (double x)
	(* x 2))
      (define (halve x)
	(/ x 2))
      (cond ((= b 0) 0)
	    ((odd? b) (+ a (ex1-17 a (- b 1))))
	    (else (double (ex1-17 a (halve b))))))

    (define (ex1-18 a b)
      (define (double x)
	(* x 2))
      (define (halve x)
	(/ x 2))
      (define (helper-iter a counter product)
	(cond ((= counter 0) product)
	      ((odd? counter) (helper-iter a (- counter 1) (+ product a)))
	      (else (helper-iter (double a) (halve counter) product))))
      (helper-iter a b 0))

    (define (prime? n)
      (define (smallest-divisor n)
	(find-divisor n 2))
      (define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
	      ((= (remainder n test-divisor) 0) test-divisor)
	      (else (find-divisor n (+ test-divisor 1))))) 
      (= n (smallest-divisor n)))

    (define (fast-prime? n)
      (define (prime-test n times)
	(cond ((= times 0) #t)
	      ((fermat-test n) (prime-test n (- times 1)))
	      (else #f)))
      (define (fermat-test n)
	(let ((a (+ 1 (random-integer (- n 1)))))
	  ;; (= (expmod a n n) a)
	  (= (modulo (fast-expt-iter a n) n) (modulo a n))
	  ))
      (define (expmod base exp m)
	(cond ((= exp 0) 1)
	      ((even? exp) (remainder (square (expmod base (/ exp 2) m))
				      m))
	      (else (remainder (* base (expmod base (- exp 1) m))
			       m))))
      (cond ((= n 2) #t)
	    (else (prime-test n 5))))))
