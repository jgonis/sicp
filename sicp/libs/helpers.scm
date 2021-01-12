(define-library (libs helpers)
  (export time-taken
	  identity
	  increment
	  decrement)
  (import (scheme base)
	  (scheme time))
  (begin
    (define (time-taken thunk)
      (let ((start (current-jiffy)))
	(thunk)
	(* 1.0 (/ (- (current-jiffy) start)
		  (jiffies-per-second)))))
    (define (identity x) x)
    (define (increment x) (+ x 1))
    (define (decrement x) (- x 1))))
