(define-library (ch2 point)
  (export make-point
	  equal-point
	  print-point
	  pt-x
	  pt-y)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda))
  (begin
    (define (make-point x y)
      (cons x y))
    (define (pt-x point)
      (car point))
    (define (pt-y point)
      (cdr point))
    
    (define (equal-point p1 p2)
      (and (= (pt-x p1) (pt-x p2))
	   (= (pt-y p1) (pt-y p2))))
    
    (define print-point
      (case-lambda
       ((pt) (print-point pt #t))
       ((pt print-newline)
	(display "(")
	(display (pt-x pt))
	(display " . ")
	(display (pt-y pt))
	(display ")")
	(if print-newline
	    (newline)))))))
