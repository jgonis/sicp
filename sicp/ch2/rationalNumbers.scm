(define-library (ch2 rationalNumbers)
  (export make-rational
	  add-rational
	  sub-rational
	  mult-rational
	  div-rational
	  equal-rational
	  print-rational)
  (import (scheme base)
          (scheme write))
  (begin
    (define (make-rational numerator denominator)
      (cons numerator denominator))
    
    (define (j-numerator rational)
      (car rational))
    
    (define (j-denominator rational)
      (cdr rational))
    
    (define (add-rational x y)
      (make-rational (+ (* (j-numerator x) (j-denominator y))
			(* (j-numerator y) (j-denominator x)))
		     (* (j-denominator x) (j-denominator y))))
    
    (define (sub-rational x y)
      (make-rational (- (* (j-numerator x) (j-denominator y))
			(* (j-numerator y) (j-denominator x)))
		     (* (j-denominator x) (j-denominator y))))
    
    (define (mult-rational x y)
      (make-rational (* (j-numerator x) (j-numerator y))
		     (* (j-denominator x) (j-denominator y))))
    
    (define (div-rational x y)
      (make-rational (* (j-numerator x) (j-denominator y))
		     (* (j-denominator x) (j-numerator y))))
    
    (define (equal-rational x y)
      (= (* (j-numerator x) (j-denominator y))
	 (* (j-denominator x) (j-numerator y))))
    
    (define (print-rational rational)
      (newline)
      (display (j-numerator rational))
      (display "/")
      (display (j-denominator rational)))))
