(define-library (ch2 intervals)
  (export make-interval
	  equal-segment
	  print-segment
	  segment-midpoint)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda))
  (begin
    (define (make-interval p1 p2)
      (cond ((equal-point p1 p2)
	     (error "interval can't be constructed from 2 equal points" p1 p2))
	    (else (cons p1 p2))))))
