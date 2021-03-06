(define-library (ch2 tree-library)
  (export count-leaves
	  deep-reverse
	  fringe)
  (import (scheme base))
  (begin
    (define (count-leaves tree)
      (cond ((null? tree) 0)
	    ((not (pair? tree)) 1)
	    (else (+ (count-leaves (car tree))
		     (count-leaves (cdr tree))))))

    (define (deep-reverse tree)
      (define (helper tree result)
	(cond ((null? tree) result)
	      ((not (pair? tree)) tree)
	      (else (helper (cdr tree)
			    (cons (helper (car tree) '())
				  result)))))
      (helper tree '()))

    (define (fringe tree)
      (cond ((null? tree) '())
	    ((not (pair? tree)) tree)
	    (else (cons (fringe (car tree))
			(fringe (cdr tree)))))))) 
