(define-library (ch2 alt-intervals)
  (export make-interval
	  lower-bound
	  upper-bound)
  (import (scheme base)
	  (scheme write)
	  (scheme case-lambda))
  (begin
        (define (make-interval lower upper)
      (let ((vec (make-vector 2)))
	(cond ((> lower upper)
	       ;; (error "interval can't be constructed with lower bound > upper bound"
	     ;; 	    lower
	     ;; 	    upper))
	       (begin
		 (vector-set! vec 0 upper)
		 (vector-set! vec 1 lower)
		 vec))
	      (else (begin
		      (vector-set! vec 0 lower)
		      (vector-set! vec 1 upper)
		      vec)))))
    
    (define (lower-bound interval)
      (vector-ref interval 0))

    (define (upper-bound interval)
      (vector-ref interval 1))))
