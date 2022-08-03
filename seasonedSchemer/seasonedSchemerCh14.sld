(include "../littleSchemer/littleSchemerCh1.sld")
(include "../littleSchemer/littleSchemerCh4.sld")
(include "../littleSchemer/littleSchemerCh5.sld")
(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch14)
  (export leftmost
          letcc-leftmost
          rember1*
          letcc-rember1*
          depth*)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define (leftmost l)
      (cond ((null? l) '())
            ((atom? (car l)) (car l))
            (else (let ((leftmost-car (leftmost (car l))))
                    (cond ((atom? leftmost-car) leftmost-car)
                          (else (leftmost (cdr l))))))))

    (define letcc-leftmost
      (lambda (l)
        (letcc skip
               (letrec ((lm (lambda (l)
                              (cond ((null? l) '())
                                    ((atom? (car l)) (skip (car l)))
                                    (else (begin
                                            (lm (car l))
                                            (lm (cdr l))))))))
                 (lm l)))))
    
    (define rember1*
      (lambda (a l)
        (letrec ((r (lambda (l)
                      (cond ((null? l) '())
                            ((atom? (car l)) (cond ((eq? (car l) a) (cdr l))
                                                   (else (cons (car l)
                                                               (r (cdr l))))))
                            (else (let ((r-car-l (r (car l))))
                                    (cond ((eqlist? r-car-l
                                                    (car l))
                                           (cons (car l) (r (cdr l))))
                                          (else (cons r-car-l
                                                      (cdr l))))))))))
          (r l))))

    (define letcc-rember1*
      (lambda (a l)
        (letrec ((rm (lambda (a l oh)
                       (cond ((null? l) (oh 'no))
                             ((atom? (car l)) (if (eq? (car l) a)
                                                  (cdr l)
                                                  (cons (car l)
                                                        (rm a (cdr l) oh))))
                             (else (let ((ret-val (letcc oh (rm a (car l) oh))))
                                     (cond ((atom? ret-val) (cons (car l)
                                                                  (rm a (cdr l) oh)))
                                           (else (cons ret-val (cdr l))))))))))
          (let ((new-l (letcc oh (rm a l oh))))
            (cond ((atom? new-l) l)
                  (else new-l))))))
    
    (define depth*
      (lambda (l)
        (cond ((null? l) 1)
              ((atom? (car l)) (depth* (cdr l)))
              (else (max (depth* (cdr l))
                         (add1 (depth* (car l))))))))))
