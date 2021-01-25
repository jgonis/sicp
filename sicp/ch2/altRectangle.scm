(define-library (ch2 altRectangle)
  (export make-rectangle
	  get-top-left
	  get-bottom-right
	  get-rect-height
	  get-rect-width)
  (import (scheme base)
	  (ch2 point))
  (begin
    (define (alt-make-rectangle top-left-pt width height)
      (cond ((<= width 0) (error "Cannot create rect with 0 width" width))
	    ((<= height 0) (error "Cannot create rect with 0 height" height))
	    (else (cons top-left-pt (cons width height)))))
    
    (define (get-top-left rect)
      (car rect))
    (define (get-bottom-right rect)
      (let* ((left-x (pt-x (car rect)))
	     (top-y (pt-y (car rect)))
	     (right-x (+ left-x (car (cdr rect))))
	     (bottom-y (+ top-y (cdr (cdr rect)))))
	(make-point right-x bottom-y)))
    
    (define (get-rect-width rect)
      (car (cdr rect)))
    
    (define (get-rect-height rect)
      (cdr (cdr rect)))))
