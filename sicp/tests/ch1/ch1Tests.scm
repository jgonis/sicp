(define-library (tests ch1 ch1Tests)
  (export ch1-tests
          check-exception)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (ch1 ch1)
	  (libs fp-compare)
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
      (test-end "ex1-11-tests"))))
