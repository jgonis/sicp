(include "littleSchemerCh1.sld")
(define-library (little-schemer ch2)
  (export lat?
          member?)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1))
  (begin
    (define lat?
      (lambda (l)
        (cond ((null? l) #t)
              ((atom? (car l)) (lat? (cdr l)))
              (else #f))))
    (define member?
      (lambda (a lat)
        (cond ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else (member? a (cdr lat))))))))


