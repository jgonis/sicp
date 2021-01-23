(define-library (ch2 rectangle)
  (export make-rectangle
	  equal-rectangle
	  print-rectangle
	  rectangle-perimeter
	  rectangle-area)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (ch2 point))
  (begin
    (define (make-rectangle x y)
      (cons x y))
    (define (get-x point)
      (car point))
    (define (get-y point)
      (cdr point))
    
    (define (equal-rectangle p1 p2)
      (and (= (get-x p1) (get-x p2))
	   (= (get-y p1) (get-y p2))))

    (define (rectangle-area rect) 1)

    (define (rectangle-perimeter rect) 1)
    
    (define print-rectangle
      (case-lambda
       ((pt) (print-rectangle pt #t))
       ((pt print-newline)
	(display "(")
	(display (get-x pt))
	(display " . ")
	(display (get-y pt))
	(display ")")
	(if print-newline
	    (newline)))))))
