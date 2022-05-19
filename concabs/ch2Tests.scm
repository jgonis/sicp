(define-library (concabs-ch2Tests)
  (export ch2-tests)
  (import (scheme base)
	  (scheme write)
	  (scheme case-lambda)
	  (concabs-ch2)
	  (concabs-helpers)
	  (srfi 64)
	  (srfi 1))
  (begin    
    (define ch2-tests
      (lambda ()
	(ex2-1-tests)
	;;If using Gauche scheme, uncomment this line to avoid the
	;;test count continuing to increase
	(test-runner-reset (test-runner-current))))

    (define (ex2-1-tests)
      (test-begin "ex2-1")
      (for-each (lambda (y)
		  (for-each (lambda (x) (test-equal (ex2-1 x y) (expt x y)))
			    (iota 50)))
		  (iota 50))
      (test-end "ex2-1"))))
      
