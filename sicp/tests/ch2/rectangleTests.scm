(define-library (tests ch2 rectangleTests)
  (export rectangle-tests)
  (import (scheme base)
          (scheme write)
	  (ch2 point)
	  (ch2 rectangle)
	  (ch2 rectangleFunctions)
	  (srfi 64))
  (begin    
    (define rectangle-tests
      (lambda ()
        (rectangle-equal-tests)
	(rectangle-area-tests)
	(rectangle-perimeter-tests)
	(rectangle-error-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    
    (define (rectangle-equal-tests)
      (test-begin "rectangle-equal-tests")
      (let ((rect1 (make-rectangle (make-point 0 0) 5 5))
	    (rect2 (make-rectangle (make-point 0 0) 5 5))
	    (rect3 (make-rectangle (make-point 0 0) 4 4)))
	(test-assert (equal-rectangle rect1 rect2))
	(test-assert (equal-rectangle rect2 rect1))
	(test-assert (not (equal-rectangle rect2 rect3)))
	(test-assert (not (equal-rectangle rect3 rect2))))
      (test-end "rectangle-equal-tests")) 

    (define (rectangle-area-tests)
      (test-begin "rectangle-area-tests")
      (let ((rect1 (make-rectangle (make-point 0 0) 5 5)))
	(test-equal 25 (rectangle-area rect1)))
      (test-end "rectangle-area-tests"))

    (define (rectangle-perimeter-tests)
      (test-begin "rectangle-perimeter-tests")
      (let ((rect1 (make-rectangle (make-point 0 0) 5 5))
	    (rect2 (make-rectangle (make-point 0 0) 4 5)))
	(test-equal 20 (rectangle-perimeter rect1))
	(test-equal 18 (rectangle-perimeter rect2)))
      (test-end "rectangle-perimeter-tests"))

    (define (rectangle-error-tests)
      (test-begin "rectangle-error-tests")
      (test-error (make-rectangle (make-point 0 0) -1 5))
      (test-error (make-rectangle (make-point 0 0) 1 -5))
      (test-error (make-rectangle (make-point 0 0) -1 -5))
      (test-end "rectangle-error-tests"))))


