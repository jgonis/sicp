(include "littleSchemerCh1.sld")
(include "littleSchemerCh4.sld")
(define-library (little-schemer ch5)
  (export rember*
          insertR*
          occurs*
          subst*)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4))
  (begin
    (define rember*
      (lambda (a l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
              ((atom? (car l)) (cons (car l)
                                     (rember* a (cdr l))))
              (else (cons (rember* a (car l))
                          (rember* a (cdr l)))))))
    (define insertR*
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? old (car l)))
               (cons (car l)
                     (cons new
                           (insertR* new
                                     old
                                     (cdr l)))))
              ((atom? (car l)) (cons (car l)
                                     (insertR* new
                                               old
                                               (cdr l))))
              (else (cons (insertR* new
                                    old
                                    (car l))
                          (insertR* new
                                    old
                                    (cdr l)))))))
    (define occurs*
      (lambda (a l)
        (cond ((null? l) 0)
              ((and (atom? (car l)) (eq? a (car l))) (add1 (occurs* a (cdr l))))
              ((atom? (car l)) (occurs* a (cdr l)))
              (else (j+ (occurs* a (car l))
                        (occurs* a (cdr l)))))))
    (define subst*
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? old (car l))) (cons new
                                                             (subst* new
                                                                     old
                                                                     (cdr l))))
              ((atom? (car l)) (cons (car l)
                                     (subst* new
                                             old
                                             (cdr l))))
              (else (cons (subst* new
                                  old
                                  (car l))
                          (subst* new
                                  old
                                  (cdr l)))))))))



