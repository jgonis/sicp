(include "littleSchemerCh1.sld")
(include "littleSchemerCh2.sld")
(include "littleSchemerCh3.sld")
(define-library (little-schemer ch7)
  (export set?
          make-set
          alt-make-set
          subset?
          eqset?
          intersect?
          intersect
          union
          set-difference
          intersect-all
          a-pair?
          first
          second
          build-pair
          all-pairs?
          relation?
          fun?
          revrel
          fully-fun?
          alt-fully-fun?
          one-to-one?)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch2)
          (little-schemer ch3))
  (begin
    (define set?
      (lambda (lat)
        (cond ((null? lat) #t)
              ((member? (car lat) (cdr lat)) #f)
              (else (set? (cdr lat))))))
    (define make-set
      (lambda (lat)
        (cond ((null? lat) '())
              ((member? (car lat) (cdr lat)) (make-set (cdr lat)))
              (else (cons (car lat)
                          (make-set (cdr lat)))))))
    (define alt-make-set
      (lambda (lat)
        (cond ((null? lat) '())
              (else (cons (car lat)
                          (alt-make-set (multirember (car lat)
                                                     (cdr lat))))))))
    (define subset?
      (lambda (set1 set2)
        (cond ((null? set1) #t)
              (else (and (member? (car set1) set2)
                         (subset? (cdr set1) set2))))))
    (define eqset?
      (lambda (set1 set2)
        (and (subset? set1 set2) (subset? set2 set1))))
    (define intersect?
      (lambda (set1 set2)
        (cond ((null? set1) #f)
              (else (or (member? (car set1) set2)
                        (intersect? (cdr set1) set2))))))
    (define intersect
      (lambda (set1 set2)
        (cond ((null? set1) '())
              ((member? (car set1) set2)
               (cons (car set1)
                     (intersect (cdr set1) set2)))
              (else (intersect (cdr set1) set2)))))
    (define union
      (lambda (set1 set2)
        (cond ((null? set1) set2)
              (else (cons (car set1)
                          (union (cdr set1)
                                 (multirember (car set1) set2)))))))
    (define set-difference
      (lambda (set1 set2)
        (cond ((null? set1) '())
              ((member? (car set1) set2) (set-difference (cdr set1)
                                                         set2))
              (else (cons (car set1)
                          (set-difference (cdr set1) set2))))))
    (define intersect-all
      (lambda (l-set)
        (cond ((null? (cdr l-set)) (car l-set))
              (else (intersect (car l-set)
                               (intersect-all (cdr l-set)))))))
    (define a-pair?
      (lambda (x)
        (cond ((atom? x) #f)
              ((null? x) #f)
              ((null? (cdr x)) #f)
              ((null? (cdr (cdr x))) #t)
              (else #f))))
    (define first
      (lambda (pair)
        (car pair)))
    (define second
      (lambda (pair)
        (car (cdr pair))))
    (define seconds
      (lambda (l-of-pairs)
        (cond ((null? l-of-pairs) (quote ()))
              (else (cons (second (car l-of-pairs))
                          (seconds (cdr l-of-pairs)))))))
    (define build-pair
      (lambda (sexp1 sexp2)
        (cons sexp1
              (cons sexp2
                    (quote ())))))
    (define revpair
      (lambda (pair)
        (build-pair (second pair) (first pair))))
    (define all-pairs?
      (lambda (l)
        (cond ((null? l) #t)
              ((a-pair? (car l)) (all-pairs? (cdr l)))
              (else #f))))
    (define relation?
      (lambda (l)
        (and (set? l) (all-pairs? l))))
    (define fun?
      (lambda (rel)
        (set? (firsts rel))))
    (define revrel
      (lambda (rel)
        (cond  ((null? rel) '())
               (else (cons (revpair (car rel))
                           (revrel (cdr rel)))))))
    (define fully-fun?
      (lambda (rel)
        (and (fun? rel) (fun? (revrel rel)))))
    (define alt-fully-fun?
      (lambda (rel)
        (and (set? (firsts rel)) (set? (seconds rel)))))
    (define one-to-one?
      (lambda (rel)
        (and (set? (firsts rel)) (set? (seconds rel)))))))



