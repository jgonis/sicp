(define-library (tests ch2 intervalTests)
  (export interval-tests
	  interval-op-tester)
  (import (scheme base)
          (scheme write)
	  (libs helpers)
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
	(interval-mul-test)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))
    
    (define (make-interval-error-test)
      (test-begin "make-interval-error-test")
      ;; (test-error #t (make-interval 5 4))
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
      (test-end "interval-div-test"))

    (define (interval-mul-test)
      (test-begin "interval-mul-test")
      ;; (let ((intervals (generate-intervals)))
      ;; 	(interval-op-tester mul-interval mul-interval-alt intervals))
      (test-end "interval-mul-test"))
    
    (define (generate-intervals) 
      (define test-list '())       
      (define test-data 
        (cons (list 0 1 2 ;; 3 4 5	;; -6 -7 -8 -9 -10
       		    ) 
              (list 5 4 3 ;; 2 1 ;; 0 -1 -2 -3 -4 -5
       		    )))
      (for-each (lambda (first)
		  (for-each (lambda (second)
			      (set! test-list
				    (cons (cons first second)
					  test-list)))
			    (cdr test-data)))
		(car test-data))
      test-list) 


    (define (interval-op-tester op op2 test-intervals)
      (define (helper subject-index current-index)
	(cond ((= subject-index (length test-intervals)) () (display "done"))
	      ((= current-index
		  (length test-intervals)) (begin (display "increment subject")
						  (newline)
						  (helper (+ subject-index 1)
							  0)))
	      (else (begin
		      (let ((subject (list-ref test-intervals subject-index))
			    (current (list-ref test-intervals current-index)))
			(if (not (equal-interval (op subject current)
						 (op2 subject current)))
			    (begin
			      (display "no match")
			      (newline)
			      (display "subject: ")
			      (print-interval subject)
			      (newline)
			      (display "current: ")
			      (print-interval current)
			      (newline)
			      (newline)))
			(helper subject-index (+ current-index 1)))))))
      (helper 0 0)))) 


