(include "littleSchemerCh1.sld")
(define-library (little-schemer ch3)
  (export rember
          firsts
          insertR
          insertL
          subst
          subst2
          multirember
          multiinsertR
          multiinsertL
          multisubst)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1))
  (begin
    (define rember
      (lambda (a lat)
        (cond ((null? lat) '())
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))
    (define firsts
      (lambda (l)
        (cond ((null? l) (quote ()))
              (else (cons (car (car l))
                          (firsts (cdr l)))))))
    (define insertR
      (lambda (new old lat)
        (cond ((null? lat) (quote ()))
              ((eq? old (car lat)) (cons (car lat)
                                         (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))
    (define insertL
      (lambda (new old lat)
        (cond ((null? lat) (quote ()))
              ((eq? old (car lat)) (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))
    (define subst
      (lambda (new old lat)
        (cond ((null? lat) (quote ()))
              ((eq? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))
    (define subst2
      (lambda (new old1 old2 lat)
        (cond ((null? lat) (quote ()))
              ((or (eq? old1 (car lat))
                   (eq? old2 (car lat)))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new old1 old2 (cdr lat)))))))
    (define multirember
      (lambda (a lat)
        (cond ((null? lat) (quote ()))
              ((eq? a (car lat)) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))
    (define multiinsertR
      (lambda (new old lat)
        (cond ((null? lat) (quote ()))
              ((eq? old (car lat))
               (cons (car lat)
                     (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertR new old (cdr lat)))))))
    (define multiinsertL
      (lambda (new old lat)
        (cond ((null? lat) (quote ()))
              ((eq? old (car lat))
               (cons new (cons (car lat) (multiinsertL new
                                                        old
                                                        (cdr lat)))))
              (else (cons (car lat) (multiinsertL new
                                                   old
                                                   (cdr lat)))))))
    (define multisubst
      (lambda (new old lat)
        (cond ((null? lat) (quote ()))
              ((eq? old (car lat)) (cons new (multisubst new
                                                         old
                                                         (cdr lat))))
              (else (cons (car lat) (multisubst new
                                                old
                                                (cdr lat)))))))))


