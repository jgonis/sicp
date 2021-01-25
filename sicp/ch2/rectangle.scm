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
    (define (make-rectangle top-left-pt width height)
      (cond ((<= width 0) (error "Cannot create rect with 0 width" width))
	    ((<= height 0) (error "Cannot create rect with 0 height" height))
	    (else
	     (let* ((left-x (pt-x top-left-pt))
		    (top-y (pt-y top-left-pt))
		    (right-x (+ left-x width))
		    (bottom-y (+ left-x height))
		    (bottom-right-pt (make-point right-x bottom-y)))
	       (cons top-left-pt bottom-right-pt)))))
	    
    (define (get-top-left rect)
      (car rect))
    (define (get-bottom-right rect)
      (cdr rect))
    
    (define (get-rect-width rect)
      (let ((right-rect-x (pt-x (get-bottom-right rect)))
	    (left-rect-x (pt-x (get-top-left rect))))
	(- right-rect-x left-rect-x)))
    
    (define (get-rect-height rect)
      (let ((top-rect-y (pt-y (get-top-left rect)))
	    (bottom-rect-y (pt-y (get-bottom-right rect))))
	(- bottom-rect-y top-rect-y)))
      
    (define (equal-rectangle r1 r2)
      (and (equal-point (get-top-left r1) (get-top-left r2))
	   (equal-point (get-bottom-right r1) (get-bottom-right r2))))

    (define (rectangle-area rect)
      (* (get-rect-width rect) (get-rect-height rect)))

    (define (rectangle-perimeter rect)
      (+ (* 2 (get-rect-width rect))
	 (* 2 (get-rect-height rect))))
    
    (define print-rectangle
      (case-lambda
       ((r) (print-rectangle r #t))
       ((r print-newline)
	(display "top-left: ")
	(print-point (get-top-left r #f))
	(display " width: ")
	(display (get-rect-width r))
	(display " height: ")
	(display (get-rect-height r))
	(display " bottom-right: ")
	(print-point (get-bottom-right r #f))
	(if print-newline
	    (newline)))))))
