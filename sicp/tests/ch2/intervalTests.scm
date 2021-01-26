(define-library (tests ch2 intervalTests)
  (export interval-tests)
  (import (scheme base)
          (scheme write)
	  (ch2 intervals)
	  (srfi 64))
  (begin    
    (define interval-tests
      (lambda ()
	(interval-test)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    
    (define (interval-test)
      (test-begin "interval-test")
      (test-equal 0 0)
      (test-end "rectangle-equal-tests"))))


