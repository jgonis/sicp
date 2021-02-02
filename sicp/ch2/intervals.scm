(define-library (ch2 intervals)
  (export make-interval
	  lower-bound
	  upper-bound
	  interval-width
	  equal-interval
	  print-interval)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda))
  (begin
    (define (make-interval lower upper)
      (cond ((> lower upper)
	     (error "interval can't be constructed with lower bound > upper bound"
		    lower
		    upper))
	    (else (cons lower upper))))
    
    (define (lower-bound interval)
      (car interval))
    
    (define (upper-bound interval)
      (cdr interval))

    (define (interval-width interval)
      (- (upper-bound interval) (lower-bound interval)))
    
    (define (equal-interval x y)
      (and (= (lower-bound x)
	      (lower-bound y))
	   (= (upper-bound x)
	      (upper-bound y))))
    
    (define print-interval
      (case-lambda ((interval) (print-interval interval #t))
		   ((interval print-newline)
		    (display "lower: ")
		    (display (lower-bound interval))
		    (display " upper: ")
		    (display (upper-bound interval))
		    (if print-newline
			(newline))))))) 
