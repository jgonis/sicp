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
          multisubst
          add1
          sub1
          j+
          j-
          addtup
          j*
          tup+
          j>
          j<
          j=
          alt-j=
          ^
          j/
          j-length
          pick
          rempick
          no-nums
          all-nums
          eqan?
          occurs)
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
    (define add1
      (lambda (n)
        (+ 1 n)))
    (define sub1
      (lambda (n)
        (- n 1)))
    (define j+
      (lambda (x y)
        (cond ((zero? y) x)
              (else (j+ (add1 x) (sub1 y))))))
    (define j-
      (lambda (x y)
        (cond ((zero? y) x)
              (else (j- (sub1 x) (sub1 y))))))
    (define addtup
      (lambda (tuple)
        (cond ((null? tuple) 0)
              (else (j+ (car tuple) (addtup (cdr tuple)))))))
    (define j*
      (lambda (x y)
        (cond ((zero? y) 0)
              (else (j+ x (j* x (sub1 y)))))))
    (define tup+
      (lambda (tup1 tup2)
        (cond ((null? tup1) tup2)
              ((null? tup2) tup1)
              (else (cons (j+ (car tup1) (car tup2))
                          (tup+ (cdr tup1) (cdr tup2)))))))
    (define j>
      (lambda (x y)
        (cond ((zero? x) #f)
              ((zero? y) #t)
              (else (j> (sub1 x) (sub1 y))))))
    (define j<
      (lambda (x y)
        (cond ((zero? y) #f)
              ((zero? x) #t)
              (else (j< (sub1 x) (sub1 y))))))
    (define j=
      (lambda (x y)
        (cond ((zero? x) (zero? y))
              ((zero? y) #f)
              (else (j= (sub1 x) (sub1 y))))))
    (define alt-j=
      (lambda (x y)
        (cond ((j< x y) #f)
              ((j> x y) #f)
              (else #t))))
    (define ^
      (lambda (x y)
        (cond ((zero? y) 1)
              (else (j* x (^ x (sub1 y)))))))
    (define j/
      (lambda (x y)
        (cond ((j< x y) 0)
              (else (add1 (j/ (j- x y) y))))))
    (define j-length
      (lambda (lat)
        (cond ((null? lat) 0)
              (else (add1 (j-length (cdr lat)))))))
    (define pick
      (lambda (n lat)
        (cond ((zero? (sub1 n)) (car lat))
              (else (pick (sub1 n) (cdr lat))))))
    (define rempick
      (lambda (n lat)
        (cond ((zero? (sub1 n)) (cdr lat))
              (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
    (define no-nums
      (lambda (lat)
        (cond ((null? lat) (quote ()))
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))
    (define all-nums
      (lambda (lat)
        (cond ((null? lat) (quote ()))
              ((number? (car lat)) (cons (car lat)
                                         (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))
    (define eqan?
      (lambda (a1 a2)
        (cond ((and (number? a1) (number? a2)) (j= a1 a2))
              ((and (atom? a1) (atom? a2)) (eq? a1 a2))
              (else #f))))
    (define occurs
      (lambda (a lat)
        0))))

;;You can't ask for the car of a null list and you can't ask for
;;the cdr of a null list


