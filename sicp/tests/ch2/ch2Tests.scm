(define-library (tests ch2 ch2Tests)
  (export ch2-tests)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (ch2 ch2)
	  (libs fp-compare)
	  (libs helpers)
          (srfi 64)
	  (srfi 1))
  (begin    
    (define ch2-tests
      (lambda ()
        (test-func-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    (define (test-func-tests)
      (test-begin "test-func-tests")
      (test-equal (test-func) 1)
      (test-end "test-func-tests"))))

