(define-library (ch2 geometry)
  (export make-segment
	  make-point
	  equal-segment
	  equal-point
	  print-segment
	  print-point
	  midpoint-segment)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda))
  (begin
    (define (make-segment p1 p2)
      (cons p1 p2))
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

    (define (midpoint-segment seg)
      (let* ((start-point (get-start-point seg))
	     (end-point (get-end-point seg))
	     (start-x (get-x start-point))
	     (end-x (get-x end-point))
	     (half-x-diff (/ (- end-x start-x) 2))
	     (new-x (+ start-x half-x-diff))
	     (start-y (get-y start-y))
	     (end-y (get-y end-point))
	     (half-y-diff (/ (- end-y start-y) 2))
	     (new-y (+ start-y half-y-diff)))
	(make-point new-x new-y)))

    (define (make-point x y)
      (cons x y))
    (define (get-x point)
      (car point))
    (define (get-y point)
      (cdr point))
    
    (define (equal-point p1 p2)
      (and (= (get-x p1) (get-x p2))
	   (= (get-y p1) (get-y p2))))
    
    (define print-point
      (case-lambda
       ((pt) (print-point pt #t))
       ((pt print-newline)
	(display "(")
	(display (get-x pt))
	(display " . ")
	(display (get-y pt))
	(display ")")
	(if print-newline
	    (newline)))))))
