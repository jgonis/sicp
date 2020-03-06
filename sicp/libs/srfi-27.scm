(define-library (libs srfi-27)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  (import (scheme base))
  (begin
    (define default-random-source (make java.util.Random))
    (define (random-integer n)
      (default-random-source:nextInt n))
    (define (random-real) 1)
    (define (make-random-source) (make java.util.Random))
    (define (random-source?) #f) 
    (define (random-source-state-ref s) #f)
    (define (random-source-state-set! s seed)
      (s:setSeed seed))
    (define (random-source-randomize! s)
      s)
    (define (random-source-pseudo-randomize! s i j)
      s)
    (define (random-source-make-integers s)
      (lambda (n) (s:nextInt n)))
    (define (random-source-make-reals s)
      (lambda () (s:nextDouble)))))
