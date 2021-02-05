;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions

(define-library (ch2 ch2)
  (export alt-cons
	  alt-car
	  alt-cdr
	  numeric-cons
	  numeric-car
	  numeric-cdr
	  last-pair
	  j-reverse
	  count-change
	  same-parity
	  square-list
	  alt-square-list
	  j-for-each)
  (import (scheme base)
          (scheme write)
	  (libs fp-compare)
	  (libs helpers))
  (begin    
    (define (alt-cons x y)
      (lambda (m) (m x y)))
    
    (define (alt-car pair)
      (pair (lambda (x y) x)))
    
    (define (alt-cdr pair)
      (pair (lambda (x y) y)))
    
    (define (numeric-cons x y)
      (* (expt 2 x) (expt 3 y)))
    
    (define (numeric-car pair)
      (define (iter num counter)
	(cond ((even? num) counter)
	      (else (iter (/ num 2) (+ counter 1)))))
      (iter pair 0))
    
    (define (numeric-cdr pair)
      (define (iter num counter)
	(cond ((not (= 0 (remainder num 3))) counter)
	      (else (iter (/ num 3) (+ counter 1)))))
      (iter pair 0))

    (define (last-pair items)
      (cond ((null? (cdr items)) items)
	    (else (last-pair (cdr items)))))
    
    (define (j-reverse items)
      (define (helper lst work-lst)
	(cond ((null? lst) work-lst)
	      (else (helper (cdr lst) (cons (car lst) work-lst)))))
      (helper items '()))

    (define (count-change amount coin-values)
      (define (cc amount coins)
	(cond ((= amount 0) 1)
	      ((or (< amount 0)
		   (no-more? coins)) 0)
	      (else (+ (cc amount
			   (except-first-denomination coins))
		       (cc (- amount
			      (first-denomination coins))
			   coins)))))
      (define (except-first-denomination coins)
	(cdr coins))
      (define (first-denomination coins)
	(car coins))
      (define (no-more? coins)
	(null? coins))
      (cc amount coin-values))

    (define (same-parity x . rest)
      (define (helper lst test-func result)
	(cond ((null? lst) (j-reverse result))
	      ((test-func (car lst)) (helper (cdr lst)
					     test-func
					     (cons (car lst) result)))
	      (else (helper (cdr lst)
			    test-func
			    result))))
      (cond ((even? x) (helper rest
			       even?
			       (list x)))
	    (else (helper rest
			  odd?
			  (list x)))))

    (define (square-list items)
      (if (null? items)
	  nil
	  (cons (square (car items))
		(square-list (cdr items)))))

    (define (alt-square-list items)
      (map (lambda (n) (* n n)) items))

    (define (j-for-each func lst)
      (cond ((not (null? lst)) (func (car lst))
	     (j-for-each func (cdr lst)))))))
