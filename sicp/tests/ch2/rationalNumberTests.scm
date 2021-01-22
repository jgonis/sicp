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
      (let ((one-half (make-rational 1 2))
	    (one-quarter (make-rational 1 4))
	    (negative-one-half (make-rational -1 2))
	    (negative-one-quarter (make-rational -1 4)))
	(test-assert (equal-rational (make-rational 3 4)
				     (add-rational one-half
						   one-quarter)))
	(test-assert (equal-rational one-half
				     (add-rational one-quarter
						   one-quarter)))
	(test-assert (equal-rational negative-one-quarter
				     (add-rational one-quarter
						   negative-one-half))))
      (test-end "rational-addition-tests"))
    (define (rational-subtraction-tests)
      (test-begin "rational-subtraction-tests")
      (let ((one-half (make-rational 1 2))
	    (three-quarters (make-rational 3 4))
	    (one-quarter (make-rational -1 -4))
	    (negative-one-quarter (make-rational -1 4))
	    (negative-one-half (make-rational -1 2)))
	(test-assert (equal-rational one-quarter
				     (sub-rational three-quarters
						   one-half)))
	(test-assert (equal-rational one-half
				     (sub-rational three-quarters
						   one-quarter)))
	(test-assert (equal-rational negative-one-quarter
				     (sub-rational one-half
						   three-quarters)))
	(test-assert (equal-rational one-quarter
				     (sub-rational negative-one-quarter
						   negative-one-half))))
      (test-end "rational-subtraction-tests"))
    (define (rational-multiplication-tests)
      (test-begin "rational-multiplication-tests")
      (let ((one-half (make-rational 1 2))
	    (one-quarter (make-rational 1 4))
	    (negative-one-half (make-rational -1 2))
	    (negative-one-quarter (make-rational -1 4)))
	(test-assert (equal-rational one-quarter
				     (mult-rational one-half
						    one-half)))
	(test-assert (equal-rational one-quarter
				     (mult-rational negative-one-half
						    negative-one-half)))
	(test-assert (equal-rational negative-one-quarter
				     (mult-rational negative-one-half
						    one-half))))
      (test-end "rational-multiplication-tests"))
    (define (rational-division-tests)
      (test-begin "rational-division-tests")
      (let ((one-half (make-rational 1 2))
	    (one-quarter (make-rational 1 4))
	    (negative-one-half (make-rational -1 2))
	    (negative-one-quarter (make-rational -1 4)))
	(test-assert (equal-rational (make-rational 2 1)
				     (div-rational one-half
						   one-quarter))))
      (test-end "rational-division-tests"))))


