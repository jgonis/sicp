(define-library (tests ch2 pointTests)
  (export point-tests)
  (import (scheme base)
          (scheme write)
	  (ch2 rationalNumbers)
	  (ch2 point)
	  (srfi 64))
  (begin    
    (define point-tests
      (lambda ()
        (point-equal-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    
    (define (point-equal-tests)
      (test-begin "point-equal-tests")
      (let ((zero-zero (make-point 0 0))
	    (one-two (make-point 1 2))
	    (negative-one-two (make-point -1 2))
	    (negative-one-negative-two (make-rational -1 -2)))
	(test-assert (equal-point (make-point 0 0)
				  zero-zero))
	(test-assert (not (equal-point one-two
				       negative-one-two))))
      (test-end "point-equal-tests"))))


