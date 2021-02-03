(define-library (ch2 intervals)
  (export make-interval
 	  make-interval-center-width
	  make-interval-center-percent
	  lower-bound
	  upper-bound)
  (import (scheme base)
	  (scheme write)
	  (scheme case-lambda))
  (begin
    (define (make-interval lower upper)
      (cond ((> lower upper)
	     ;; (error "interval can't be constructed with lower bound > upper bound"
	     ;; 	    lower
	     ;; 	    upper))
	     (cons upper lower))
	    (else (cons lower upper))))
    
    (define (lower-bound interval)
      (car interval))

    (define (upper-bound interval)
      (cdr interval)))) 
