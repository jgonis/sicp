(include "littleSchemerCh1.sld")
(include "littleSchemerCh4.sld")
(define-library (little-schemer ch5)
  (export rember*
          insertR*
          occurs*
          subst*
          insertL*
          member*?
          eqlist?
          j-equal?)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4))
  (begin
    (define rember*
      (lambda (a l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? a (car l)))
               (rember* a (cdr l)))
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
              ((and (atom? (car l)) (eq? a (car l)))
               (add1 (occurs* a (cdr l))))
              ((atom? (car l)) (occurs* a (cdr l)))
              (else (j+ (occurs* a (car l))
                        (occurs* a (cdr l)))))))
    (define subst*
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? old (car l)))
               (cons new
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
                                  (cdr l)))))))
    (define insertL*
      (lambda (new old l)
        (cond ((null? l) (quote ()))
              ((and (atom? (car l)) (eq? old (car l)))
               (cons new
                     (cons (car l)
                           (insertL* new
                                     old
                                     (cdr l)))))
              ((atom? (car l)) (cons (car l)
                                     (insertL* new
                                               old
                                               (cdr l))))
              (else (cons (insertL* new
                                    old
                                    (car l))
                          (insertL* new
                                    old
                                    (cdr l)))))))
    (define member*?
      (lambda (a l)
        (cond ((null? l) #f)
              ((and (atom? (car l)) (eq? a (car l))) #t)
              ((atom? (car l)) (member*? a (cdr l)))
              (else (or (member*? a (car l))
                        (member*? a (cdr l)))))))
    (define leftmost
      (lambda (l)
        (cond ((atom? (car l)) (car l))
              (else (leftmost (car l))))))
    (define eqlist?
      (lambda (l1 l2)
        (cond ((and (null? l1) (null? l2)) #t)
              ((or (null? l1) (null? l2)) #f)
              (else (and (j-equal? (car l1) (car l2))
                         (eqlist? (cdr l1) (cdr l2)))))))
    (define j-equal?
      (lambda (sexp1 sexp2)
        (cond ((and (atom? sexp1)
                    (atom? sexp2)) (eqan? sexp1 sexp2))
              ((or (atom? sexp1) (atom? sexp2)) #f)
              (else (eqlist? sexp1 sexp2)))))))



