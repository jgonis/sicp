(define-library (tests ch2 rationalNumberTests)
  (export rational-number-tests)
  (import (scheme base)
          (scheme write)
	  (ch2 rationalNumbers)
	  (srfi 64))
  (begin    
    (define rational-number-tests
      (lambda ()
        (rational-addition-tests)
	(rational-subtraction-tests)
	(rational-multiplication-tests)
	(rational-division-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    (define (test-func-tests)
      (test-begin "test-func-tests")
      (test-equal (test-func) 1)
      (test-end "test-func-tests"))
    
    (define (rational-addition-tests)
      (test-begin "rational-addition-tests")
      (test-end "rational-addition-tests"))
    (define (rational-subtraction-tests)
      (test-begin "rational-subtraction-tests")
      (test-end "rational-subtraction-tests"))
    (define (rational-multiplication-tests)
      (test-begin "rational-multiplication-tests")
      (test-end "rational-multiplication-tests"))
    (define (rational-division-tests)
      (test-begin "rational-division-tests")
      (test-end "rational-division-tests"))))


