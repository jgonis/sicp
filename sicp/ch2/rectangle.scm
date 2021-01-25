(define-library (ch2 rectangle)
  (export make-rectangle
	  get-top-left
	  get-bottom-right
	  get-rect-height
	  get-rect-width)
  (import (scheme base)
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

    (define (alt-make-rectangle top-left-pt width height)
      (cond ((<= width 0) (error "Cannot create rect with 0 width" width))
	    ((<= height 0) (error "Cannot create rect with 0 height" height))
	    (else (cons top-left-pt (cons width height)))))
    
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
	(- bottom-rect-y top-rect-y)))))
