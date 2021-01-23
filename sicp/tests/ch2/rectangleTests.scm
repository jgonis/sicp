(define-library (tests ch2 rectangleTests)
  (export rectangle-tests)
  (import (scheme base)
          (scheme write)
	  (ch2 point)
	  (srfi 64))
  (begin    
    (define rectangle-tests
      (lambda ()
        (rectangle-equal-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    
    (define (rectangle-equal-tests)
      (test-begin "rectangle-equal-tests")
      (let ((one-half (make-rational 1 2))
	    (one-quarter (make-rational 1 4))
	    (negative-one-half (make-rational -1 2))
	    (negative-one-quarter (make-rational -1 4)))
	(test-assert (equal-rational (make-rational 2 1)
				     (div-rational one-half
						   one-quarter))))
      (test-end "rectangle-equal-tests"))))


