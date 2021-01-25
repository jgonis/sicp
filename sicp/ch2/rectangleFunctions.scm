(define-library (ch2 rectangleFunctions)
  (export equal-rectangle
	  print-rectangle
	  rectangle-perimeter
	  rectangle-area)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (ch2 point)
	  (ch2 rectangle)
	  ;; (ch2 altRectangle)
	  )
  (begin
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
