(include "../littleSchemer/littleSchemerCh1.sld")
(include "../littleSchemer/littleSchemerCh4.sld")
(include "../littleSchemer/littleSchemerCh5.sld")
(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch14)
  (export leftmost
          rember1*
          depth*)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5)
          (seasoned-schemer ch11))
  (begin
    (define leftmost
      (lambda (l)
        (letcc skip
               (letrec ((helper
                         (lambda (l)
                           (cond ((null? l) '())
                                 ((atom? (car l)) (skip (car l)))
                                 (else (begin
                                         (helper (car l))
                                         (helper (cdr l))))))))
                 (helper l)))))
    (define rember1*
      (lambda (a l)
        (letrec ((helper
                  (lambda (l)
                    (cond ((null? l) '())
                          ((atom? (car l))
                           (cond ((eq? (car l) a) (cdr l))
                                 (else (cons (car l)
                                             (helper (cdr l))))))
                          (else (let ((result (helper (car l))))
                                  (cond ((eqlist? result
                                                  (car l))
                                         (cons (car l) (helper (cdr l))))
                                        (else (cons result
                                                    (cdr l))))))))))
          (helper l))))
    (define depth*
      (lambda (l)
        (cond ((null? l) 1)
              ((atom? (car l)) (depth* (cdr l)))
              (else (max (depth* (cdr l))
                         (add1 (depth* (car l))))))))))
