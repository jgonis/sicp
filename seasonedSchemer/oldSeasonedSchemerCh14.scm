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
    (define letcc-leftmost
      (lambda (l)
        (letcc skip
               (letrec
                   ((lm
                     (lambda (l)
                       (cond ((null? l) '())
                             (else (let ((c-l (car l)))
                                     (cond ((atom? c-l)
                                            (skip c-l))
                                           (else (begin
                                                   (lm c-l)
                                                   (lm (cdr l)))))))))))
                 (lm l)))))
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
    (define letcc-rember1*
      (lambda (a l)
        (letrec ((helper
                  (lambda (a l out)
                    (cond ((null? l) (out 'no))
                          ((atom? (car l))
                           (cond ((eq? (car l) a) (cdr l))
                                 (else (cons (car l)
                                             (helper a (cdr l) out)))))
                          (else (let ((result
                                       (letcc out
                                              (helper a (car l) out))))
                                  (cond ((atom? result)
                                         (cons (car l)
                                               (helper a (cdr l) out)))
                                        (else (cons result
                                                    (cdr l))))))))))
          (let ((result (letcc out
                               (helper a l out))))
            (cond ((atom? result) l)
                  (else result))))))
    
    (define depth*
      (lambda (l)
        (cond ((null? l) 1)
              ((atom? (car l)) (depth* (cdr l)))
              (else (max (depth* (cdr l))
                         (add1 (depth* (car l)))))))))) 
