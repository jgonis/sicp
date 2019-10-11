(include "littleSchemerCh1.sld")
(define-library (little-schemer ch5)
  (export rember*
          insertR*)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1))
  (begin
    (define rember*
      (lambda (a l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
              ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
              (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
    (define insertR*
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? old (car l)))
               (cons (car l)
                     (cons new (insertR* new old (cdr l)))))
              ((atom? (car l)) (cons (car l) (insertR* new old (cdr l))))
              (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))))



