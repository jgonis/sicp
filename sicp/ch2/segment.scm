(define-library (ch2 segment)
  (export make-segment
	  equal-segment
	  print-segment
	  segment-midpoint)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (ch2 point))
  (begin
    (define (make-segment p1 p2)
      (cond ((equal-point p1 p2)
	     (error "segment can't be constructed from 2 equal points" p1 p2))
	    (else (cons p1 p2))))
    
    (define (get-start-point seg)
      (car seg))
    (define (get-end-point seg)
      (cdr seg))
    
    (define (equal-segment seg1 seg2)
      (or (and (equal-point (get-start-point seg1)
			    (get-start-point seg2))
	       (equal-point (get-end-point seg1)
			    (get-end-point seg2)))
	  (and (equal-point (get-end-point seg1)
			    (get-start-point seg2))
	       (equal-point (get-start-point seg1)
			    (get-end-point seg2)))))

    (define print-segment
      (case-lambda
       ((seg) (print-segment seg #t))
       ((seg print-newline)
	(print-point (get-start-point seg) #f)
	(display " -> ")
	(print-point (get-end-point seg) #f)
	(if print-newline
	    (newline)))))

    (define (segment-midpoint seg)
      (let* ((start-point (get-start-point seg))
	     (end-point (get-end-point seg))
	     (start-x (pt-x start-point))
	     (end-x (pt-x end-point))
	     (half-x-diff (/ (- end-x start-x) 2))
	     (new-x (+ start-x half-x-diff))
	     (start-y (pt-y start-point))
	     (end-y (pt-y end-point))
	     (half-y-diff (/ (- end-y start-y) 2))
	     (new-y (+ start-y half-y-diff)))
	(make-point new-x new-y)))))
