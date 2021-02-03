(define-library (ch2 intervalFunctions)
  (export make-interval-center-width
	  make-interval-center-percent
	  add-interval
	  sub-interval
	  mul-interval
	  div-interval
	  interval-center
	  interval-width
	  equal-interval
	  print-interval)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (ch2 intervals)
	  (libs helpers))
  (begin

    (define (make-interval-center-width center tolerance)
      (make-interval (- center tolerance) (+ center tolerance)))
    
    (define (make-interval-center-percent center tolerance-percent)
      (let ((tolerance (* center tolerance-percent)))
	(make-interval (- center tolerance) (+ center tolerance))))

    
    (define (add-interval x y)
      (make-interval (+ (lower-bound x)
			(lower-bound y))
		     (+ (upper-bound x)
			(upper-bound y))))
    
    (define (sub-interval x y)
      (make-interval (- (lower-bound x)
			(upper-bound y))
		     (- (upper-bound x)
			(lower-bound y))))
    
    (define (mul-interval-old x y)
      (let ((p1 (* (lower-bound x)
		   (lower-bound y)))
	    (p2 (* (lower-bound x)
		   (upper-bound y)))
	    (p3 (* (upper-bound x)
		   (lower-bound y)))
	    (p4 (* (upper-bound x)
		   (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
		       (max p1 p2 p3 p4))))

    (define (mul-interval x y)
      (let ((lower-x (lower-bound x))
	    (upper-x (upper-bound x))
	    (lower-y (lower-bound y))
	    (upper-y (upper-bound y)))
	(cond ((and (>= lower-x 0)
		    (>= upper-x 0)
		    (>= lower-y 0)
		    (>= upper-y 0)) (make-interval (* lower-x lower-y)
		    (* upper-x upper-y)))
	      ((and (>= lower-x 0)
		    (>= upper-x 0)
		    (< lower-y 0)
		    (>= upper-y 0)) (make-interval (* upper-x lower-y)
		    (* upper-x upper-y)))
	      ((and (>= lower-x 0)
		    (>= upper-x 0)
		    (< lower-y 0)
		    (< upper-y 0)) (make-interval (* upper-x lower-y)
		    (* lower-x upper-y)))
	      ((and (< lower-x 0)
		    (>= upper-x 0)
		    (>= lower-y 0)
		    (>= upper-y 0)) (make-interval (* lower-x upper-y)
		    (* upper-x upper-y)))
	      ((and (< lower-x 0)
		    (< upper-x 0)
		    (>= lower-y 0)
		    (>= upper-y 0)) (make-interval (* lower-x upper-y)
		    (* upper-x lower-y)))
	      ((and (< lower-x 0)
		    (>= upper-x 0)
		    (< lower-y 0)
		    (>= upper-y 0)) (make-interval (min (* lower-x upper-y)
							(* upper-x lower-y))
		    (max (* upper-x upper-y)
			 (* lower-x lower-y))))
	      ((and (< lower-x 0)
		    (>= upper-x 0)
		    (< lower-y 0)
		    (< upper-y 0)) (make-interval (* upper-x lower-y)
		    (* lower-x lower-y)))
	      ((and (< lower-x 0)
		    (< upper-x 0)
		    (< lower-y 0)
		    (>= upper-y 0)) (make-interval (* lower-x upper-y)
		    (* lower-x lower-y)))
	      (else (make-interval (* upper-x upper-y)
				   (* lower-x lower-y))))))
    
    (define (div-interval x y)
      (cond ((and (<= (lower-bound y) 0)
		  (>= (upper-bound y) 0))
	     (error "Cannot divide with an interval that spans 0" y))
	    (else (mul-interval x
				(make-interval
				 (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y)))))))
    
    (define (interval-center interval)
      (+ (lower-bound interval) (interval-width)))
    
    (define (interval-width interval)
      (- (upper-bound interval) (lower-bound interval)))
    
    (define (interval-percent interval)
      (/ (/ (interval-width interval)
	    2)
	 (interval-center)))

    (define (equal-interval x y)
      (and (= (lower-bound x)
	      (lower-bound y))
	   (= (upper-bound x)
	      (upper-bound y))))

    (define print-interval
      (case-lambda ((interval) (print-interval interval #t))
		   ((interval print-newline)
		    (display "lower: ")
		    (display (lower-bound interval))
		    (display " upper: ")
		    (display (upper-bound interval))
		    (if print-newline
			(newline)))))))


(define (mult-test count)
  (define (iter current)
    (let* ((interval-1 (make-interval (- (random-integer 21) 10)
				      (- (random-integer 21) 10)))
	   (interval-2 (make-interval (- (random-integer 21) 10)
				      (- (random-integer 21) 10)))
	   (result (mul-interval interval-1 interval-2)))
      (cond ((= current count) (displayln interval-1))
	    (else (iter (+ current 1))))))
  (iter 0))
