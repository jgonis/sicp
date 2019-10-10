(define-library (little-schemer)
  (export atom?
          lat?
          member?
          rember
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
          (scheme write))
  (begin
    (define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
    (define lat?
      (lambda (l)
        (cond ((null? l) #t)
              ((atom? (car l)) (lat? (cdr l)))
              (else #f))))
    (define member?
      (lambda (a lat)
        (cond ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else (member? a (cdr lat))))))
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
                                                (cdr lat)))))))
    ))

;;You can't ask for the car of a null list and you can't ask for
;;the cdr of a null list


