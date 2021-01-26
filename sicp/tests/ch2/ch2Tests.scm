(define-library (tests ch2 ch2Tests)
  (export ch2-tests)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (ch2 ch2)
	  (tests ch2 rationalNumberTests)
	  (tests ch2 pointTests)
	  (tests ch2 segmentTests)
	  (tests ch2 rectangleTests)
	  (libs fp-compare)
	  (libs helpers)
          (srfi 64)
	  (srfi 1))
  (begin    
    (define ch2-tests
      (lambda ()
	(rational-number-tests)
	(point-tests)
	(segment-tests)
	(rectangle-tests)
	(alt-car-cdr-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    (define (alt-car-cdr-tests)
      (test-begin "alt-car-cdr-tests")
      (let ((pair (alt-cons 1 2))
	    (pair2 (alt-cons 0 3))
	    (pair3 (alt-cons 3 0)))
	(test-equal 1 (alt-car pair))
	(test-equal 2 (alt-cdr pair))
	(test-equal 0 (alt-car pair2))
	(test-equal 3 (alt-cdr pair2))
	(test-equal 3 (alt-car pair3))
	(test-equal 0 (alt-cdr pair3)))
      (test-end "alt-car-cdr-tests"))))

