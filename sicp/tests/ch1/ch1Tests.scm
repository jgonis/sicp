(define-library (tests ch1 ch1Tests)
  (export ch1-tests)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (ch1 ch1)
	  (libs fp-compare)
	  (libs helpers)
          (srfi 64)
	  (srfi 1))
  (begin    
    (define ch1-tests
      (lambda ()
        (ex1-2-tests)
        (ex1-3-tests)
        (j-abs-tests)
        (sqrt-tests j-sqrt "sqrt tests")
        (sqrt-tests alt-j-sqrt "alt-sqrt tests")
        (ex1-8-tests)
	(fib-tests)
	(ex1-11-tests)
	(j-expt-tests)
	(ex1-17-and-ex1-18-tests)
	(prime-tests-false)
	(prime-tests-true)
	(carmichael-number-tests)
	(sum-cubes-tests)
	(pi-sum-general-tests)
	(pi-sum-iter-tests)
	(general-sum-tests)
	(accumulate-tests)
	(accumulate-rec-tests)
	(sqrt-tests j-new-sqrt "j-new-sqrt tests")
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    (define (ex1-2-tests)
      (test-begin "ex1-2")
      (test-equal (ex1-2) -37/150)
      (test-end "ex1-2"))

    (define (ex1-3-tests)
      (test-begin "ex1-3")
      (test-equal (ex1-3 1 2 3) 13)
      (test-equal (ex1-3 1 3 2) 13)
      (test-equal (ex1-3 3 2 1) 13)
      (test-equal (ex1-3 3 1 2) 13)
      (test-equal (ex1-3 3 2 2) 13)
      (test-equal (ex1-3 2 3 2) 13)
      (test-equal (ex1-3 2 2 3) 13)
      (test-equal (ex1-3 3 3 3) 18)
      (test-end "ex1-3"))

    (define (j-abs-tests)
      (test-begin "j-abs tests")
      (test-equal (j-abs 1) 1)
      (test-equal (j-abs 0) 0)
      (test-equal (j-abs -1) 1)
      (test-end "j-abs tests"))

    (define (sqrt-tests sqrt-func test-name)
      (test-begin test-name)
      (test-assert "sqrt 2 test"
		   (fp-eq? 2
			   (sqrt-func 4)))
      (test-assert "small sqrt test"
		   (fp-eq? 0.0000009
                           (sqrt-func 0.00000000000081)
                           0.001))
      (test-assert "large sqrt test"
		   (fp-eq? 985881
			   (sqrt-func 971961346161)
			   0.001))
      (test-end test-name))

    (define (ex1-8-tests)
      (test-begin "ex1-8 tests")
      (test-assert "small cube root test"
		   (fp-eq? 3 (ex1-8 27)))
      (test-assert "decimal cube root test"
		   (fp-eq? 0.1 (ex1-8 0.001)))
      (test-end "ex1-8 tests"))

    (define (fib-tests)
      (test-begin "fib-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (jfib n)
			   (jfib-iter n)))
	     num-list))
      (test-end "fib-tests"))

    (define (ex1-11-tests)
      (test-begin "ex1-11-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (ex1-11 n)
			   (ex1-11-iter n)))
	     num-list))
      (test-end "ex1-11-tests"))

    (define (j-expt-tests)
      (test-begin "j-expt-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (j-expt 2 n)
			   (j-expt-iter 2 n))
	       (test-equal (j-expt-iter 2 n)
			   (fast-expt 2 n))
	       (test-equal (fast-expt 2 n)
			   (ex1-16 2 n)))
	     num-list))
      (test-end "j-expt-tests"))

    (define (ex1-17-and-ex1-18-tests)
      (test-begin "ex1-17-and-ex1-18-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (* 2 n)
			   (ex1-17 2 n)))
	     num-list)
	(map (lambda (n)
	       (test-equal (ex1-17 2 n)
			   (ex1-18 2 n)))
	     num-list))
      (test-end "ex1-17-and-ex1-18-tests"))

    (define (prime-tests-false)
      (test-begin "prime-tests-false")
      (let ((num-list (list 4 6 8 9 10 12 14 15)))
	(map (lambda (n)
	       (test-equal (prime? n)
			   (fast-prime? n)))
	     num-list))
      (test-end "prime-tests-false"))

    (define (prime-tests-true)
      (test-begin "prime-tests-true")
      (let ((prime-list (list 3 5 7 11 13 17 19 23)))
	(map (lambda (n)
	       (test-assert (and (prime? n)
				 (fast-prime? n))))
	     prime-list))
      (test-end "prime-tests-true"))

    (define (carmichael-number-tests)
      (test-begin "carmichael-number-tests")
      (let ((num-list (list 561 1105)))
	(map (lambda (n)
	       (test-assert (carmichael-number-tester n)))
	     num-list))
      (test-end "carmichael-number-tests"))

    (define (sum-cubes-tests)
      (test-begin "sum-cubes-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (sum-cubes 1 n)
			   (sum-cubes-general 1 n)))
	     num-list))
      (test-end "sum-cubes-tests"))

    (define (pi-sum-general-tests)
      (test-begin "pi-sum-general-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-assert (fp-eq? (pi-sum-general 1 n)
				    (pi-sum 1 n))))
	     num-list))
      (test-end "pi-sum-general-tests"))
    
    (define (pi-sum-iter-tests)
      (test-begin "pi-sum-iter-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-assert (fp-eq? (pi-sum-iter 1 n)
				    (pi-sum 1 n))))
	     num-list))
      (test-end "pi-sum-iter-tests"))

    (define (general-sum-tests)
      (test-begin "general-sum-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-assert
		(fp-eq? (pi-sum-general 1 n)
			(pi-sum-general 1 n general-sum-iter))))
	     num-list)
	(map (lambda (n)
	       (test-assert
		(fp-eq? (integral cube
				  0
				  1
				  0.001)
			(integral cube
				  0
				  1
				  0.001
				  general-sum-iter))))
	     num-list))
      (test-end "general-sum-tests"))
    
    (define (accumulate-tests)
      (test-begin "accumulate-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (general-sum identity
					0
					increment
					n)
			   (accumulate +
				       0
				       identity
				       0
				       increment
				       n)))
	     num-list)
	(map (lambda (n)
	       (test-equal (general-product identity
					    1
					    increment
					    n)
			   (accumulate *
				       1
				       identity
				       1
				       increment
				       n)))
	     num-list))
      (test-end "accumulate-tests"))
    
    (define (accumulate-rec-tests)
      (test-begin "accumulate-tests")
      (let ((num-list (iota 10)))
	(map (lambda (n)
	       (test-equal (accumulate-rec +
					   0
					   identity
					   0
					   increment
					   n)
			   (accumulate +
				       0
				       identity
				       0
				       increment
				       n)))
	     num-list))
      (test-end "accumulate-tests"))))
