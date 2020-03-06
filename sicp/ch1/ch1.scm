;;java -jar kawa.jar -s -r7rs
(define-library (ch1 ch1)
  (export ex-1-3)
  (import (scheme base))
  (begin

    (define (sum-of-squares x y)
      (+ (square x) (square y)))
    
    (define (ex-1-3 x y z)
      (cond ((and (<= x y) (< x z)) (sum-of-squares y z))
            ((and (< y z) (<= y x)) (sum-of-squares x z))
            (else (sum-of-squares x y))))))


(define (j-abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
