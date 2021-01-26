(define-library (tests ch2 intervalTests)
  (export interval-tests)
  (import (scheme base)
          (scheme write)
	  (ch2 intervals)
	  (ch2 intervalFunctions)
	  (srfi 64))
  (begin    
    (define interval-tests
      (lambda ()
	(make-interval-error-test)
	(interval-equal-test)
	(interval-width-test)
	(interval-div-test)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    (define (make-interval-error-test)
      (test-begin "make-interval-error-test")
      (test-error #t (make-interval 5 4))
      (test-end "make-interval-error-test"))

    (define (interval-equal-test)
      (test-begin "interval-equal-test")
      (let ((i1 (make-interval 1 3))
	    (i2 (make-interval 1 3))
	    (i3 (make-interval 1 2))
	    (i4 (make-interval 2 3)))
	(test-assert (equal-interval i1 i2))
	(test-assert (not (equal-interval i1 i3)))
	(test-assert (not (equal-interval i1 i4))))
      (test-end "interval-equal-test"))

    (define (interval-width-test)
      (test-begin "interval-width-test")
      (let ((i1 (make-interval 1 2)))
	(test-equal 1 (interval-width i1)))
      (test-end "interval-width-test"))

    (define (interval-div-test)
      (test-begin "interval-div-test")
      (test-end "interval-div-test"))))


