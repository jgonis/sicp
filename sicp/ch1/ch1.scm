;;java -jar kawa.jar -s -r7rs
;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions
;; (define (load-func)
;;   (load "/home/jgonis/code/sicp/sicp/ch1/ch1.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/fp-compare.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/timing.scm")
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
	  smallest-divisor
	  prime?
	  fast-prime?
	  search-for-primes
	  n-primes-larger-than
	  carmichael-number-tester)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (srfi 1)
	  (srfi 27)
          (libs fp-compare)
	  (libs timing))
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
    
    (define (smallest-divisor n)
      (define (next n)
	;; (+ n 1)
	(cond ((= n 2) 3)
	      (else (+ n 2)))
	)
      (define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
	      ((= (remainder n test-divisor) 0) test-divisor)
	      (else (find-divisor n (next test-divisor)))))
      (find-divisor n 2))
    
    (define (prime? n)
      (= n (smallest-divisor n)))

    (define (fast-prime? n)
      (define (prime-test n times)
	(cond ((= times 0) #t)
	      ((fermat-test n) (prime-test n (- times 1)))
	      (else #f)))
      (define (fermat-test n)
	(let ((a (+ 1 (random-integer (- n 1)))))
	  (= (expmod a n n) a)))
      ;; By taking the remainder at each recursive step we make sure
      ;; we are not dealing with huge exponents which would greatly
      ;; slow things down. Just using the regular old fast-expt
      ;; procedure would allow for the exponents to grow and the
      ;; procedure would grind to a halt.
      (define (expmod base exp m)
	(cond ((= exp 0) 1)
	      ((even? exp) (remainder (square (expmod base (/ exp 2) m))
				      m))
	      (else (remainder (* base (expmod base (- exp 1) m))
			       m))))
      (cond ((= n 2) #t)
	    (else (prime-test n 5))))

    (define (ex1-21)
      (display (smallest-divisor 199)) ;Prime so smallest divisor is itself
      (newline)
      (display (smallest-divisor 1999)) ;Prime so smallest divisor is itself
      (newline)
      (display (smallest-divisor 19999)) ;7, not prime!
      (newline))
    
    (define search-for-primes
      (case-lambda
       ((range-size) (search-for-primes 2 (+ 2 range-size)))
       ((start end)
	(let ((num-list (iota (- end start) start 2)))
	  (for-each (lambda (n) 
		      (display (time-taken (lambda () (fast-prime? n))))
		      (newline))
		    num-list)))))

    (define (n-primes-larger-than n start prime-test-method)
      (define (report-prime prime duration)
	(display "prime: ")
	(display prime)
	(display " time: ")
	(display (* 1.0 (/ duration (jiffies-per-second))))
	(newline))
      (define (iter-helper current primes-found)
	(cond ((= (length primes-found) n) primes-found)
	      (else
	       (let* ((start-time (current-jiffy))
		      (is-prime? (prime-test-method current))
		      (duration (- (current-jiffy) start-time)))
		 (cond (is-prime? (report-prime current
						duration)
				  (iter-helper (+ current 1)
					       (cons current
						     primes-found))) 
		       (else (iter-helper (+ current 1)
					  primes-found)))))))
      (iter-helper start (list)))

    (define (ex1-22)
      ;; 3 smallest primes larger than 1,000,000,000
      ;; prime: 1000000007 time: 0.0069
      ;; prime: 1000000009 time: 0.0088
      ;; prime: 1000000021 time: 0.0051
      ;; 3 smallest primes larger than 10,000,000,000
      ;; prime: 10000000019 time: 0.0211
      ;; prime: 10000000033 time: 0.0264
      ;; prime: 10000000061 time: 0.0176
      ;; 3 smallest primes larger than 100,000,000,000
      ;; prime: 100000000003 time: 0.0693
      ;; prime: 100000000019 time: 0.0678
      ;; prime: 100000000057 time: 0.0679
      ;; 3 smallest primes larger than 1,000,000,000,000
      ;; prime: 1000000000039 time: 0.2157
      ;; prime: 1000000000061 time: 0.2079
      ;; prime: 1000000000063 time: 0.2
      #t)
    
    (define (ex1-23)
      ;; 3 smallest primes larger than 1,000,000,000
      ;; prime: 1000000007 time: 0.0024
      ;; prime: 1000000009 time: 0.0032
      ;; prime: 1000000021 time: 0.0041
      ;; 3 smallest primes larger than 10,000,000,000
      ;; prime: 10000000019 time: 0.0131
      ;; prime: 10000000033 time: 0.0104
      ;; prime: 10000000061 time: 0.0179
      ;; 3 smallest primes larger than 100,000,000,000
      ;; prime: 100000000003 time: 0.0413
      ;; prime: 100000000019 time: 0.0293
      ;; prime: 100000000057 time: 0.0385
      ;; 3 smallest primes larger than 1,000,000,000,000
      ;; prime: 1000000000039 time: 0.1185
      ;; prime: 1000000000061 time: 0.108
      ;; prime: 1000000000063 time: 0.1024
      ;; The updated prime? procedure that doesn't check even divisors
      ;; is roughly twice as fast as the one that does, which is what we
      ;; would expact as it is doing 1/2 as much work.
      #t)

    (define (ex1-24)
      ;; 3 smallest primes larger than 1,000,000,000
      ;; prime: 1000000007 time: 6.8341e-5
      ;; prime: 1000000009 time: 6.4775e-5
      ;; prime: 1000000021 time: 6.5272e-5
      ;; 3 smallest primes larger than 10,000,000,000
      ;; prime: 10000000019 time: 2.23804e-4
      ;; prime: 10000000033 time: 2.24777e-4
      ;; prime: 10000000061 time: 2.28425e-4
      ;; 3 smallest primes larger than 100,000,000,000
      ;; prime: 100000000003 time: 2.63017e-4
      ;; prime: 100000000019 time: 2.63702e-4
      ;; prime: 100000000057 time: 2.66588e-4
      ;; 3 smallest primes larger than 1,000,000,000,000
      ;; prime: 1000000000039 time: 3.23397e-4
      ;; prime: 1000000000061 time: 3.22042e-4
      ;; prime: 1000000000063 time: 3.39159e-4
      #t)

    (define (ex1-25)
      ;; Our previously written exponential procedure would not work
      ;; as well as it would not reduce the numbers before hand so as
      ;; we tested every larger primes, we would be doing computations
      ;; on huge numbers which would be much slower than doing
      ;; equivalent computations on small numbers.
      #t)

    (define (ex1-26)
      
      #t)

    (define (carmichael-number-tester n)
      ;; (map (lambda (n)
      ;; (carmichael-number-tester n))
      ;; (list 561 1105 1729 2465 2821 6601))
      (define (iter-helper current)
	(cond ((= current n) #t)
	      ((= (modulo (fast-expt current n) n)
		  (modulo current n))
	       (iter-helper (+ current 1)))
	      (else #f)))
      (iter-helper 1)))) 
