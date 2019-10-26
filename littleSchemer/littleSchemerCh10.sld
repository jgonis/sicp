(include "littleSchemerCh1.sld")
(include "littleSchemerCh7.sld")
(define-library (little-schemer ch10)
  (export new-entry
          lookup-in-entry
          extend-table
          lookup-in-table
          expression-to-action
          atom-to-action
          list-to-action
          value
          meaning
          *const
          *identifier
          *cond
          *lambda
          *application
          *quote)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch7))
  (begin
    (define new-entry build-pair)
    (define lookup-in-entry
      (lambda (name entry entry-f)
        (lookup-in-entry-help name
                              (first entry)
                              (second entry)
                              entry-f)))
    (define lookup-in-entry-help
      (lambda (name names values entry-f)
       (cond ((null? names) (entry-f name))
             ((equal? name (car names)) (car values))
             (else (lookup-in-entry-help name
                                         (cdr names)
                                         (cdr values)
                                         entry-f)))))
    (define extend-table
      (lambda (entry table)
        (cons entry table)))
    (define lookup-in-table
      (lambda (name table table-f)
        (cond ((null? table) (table-f name))
              (else (lookup-in-entry name
                                (car table)
                                (lambda (name)
                                  (lookup-in-table name
                                                   (cdr table)
                                                   table-f)))))))
    (define atom-to-action
      (lambda (a)
        (cond ((number? a) *const)
              ((eq? a #t) *const)
              ((eq? a #f) *const)
              ((eq? a 'cons) *const)
              ((eq? a 'car) *const)
              ((eq? a 'cdr) *const)
              ((eq? a 'null?) *const)
              ((eq? a 'eq?) *const)
              ((eq? a 'atom?) *const)
              ((eq? a 'zero?) *const)
              ((eq? a 'add1) *const)
              ((eq? a 'sub1) *const)
              ((eq? a 'number?) *const)
              (else *identifier))))
    (define list-to-action
      (lambda (l)
        (cond ((atom? (car l))
               (cond ((eq? (car l) 'lambda) *lambda)
                     ((eq? (car l) 'cond) *cond)
                     ((eq? (car l) 'quote) *quote)
                     (else *application)))
              (else *application))))
    (define *const
      (lambda (expr table)
        (cond ((number? expr) e)
              ((eq? expr #t) #t)
              ((eq? expr #f) #f)
              (else (build-pair 'primitive e)))))
    (define *identifier
      (lambda (expr table)
        (lookup-in-table (car expr)
                         table
                         initial-table)))
    (define initial-table
      (lambda (name)
        (car (quote ()))))
    (define *cond
      (lambda (expr table)))
    (define *lambda
      (lambda (expr table)
        (build-pair 'non-primitive
                    (cons table (cdr e)))))
    (define *quote
      (lambda (expr table)
        (text-of expr)))
    (define text-of
      (lambda (expr)
        (second expr)))
    (define *application
      (lambda (expr table)))
    (define expression-to-action
      (lambda (e)
        (cond ((atom? e) (atom-to-action))
              (else (list-to-action e)))))
    (define value
      (lambda (e)
        (meaning e '())))
    (define meaning
      (lambda (e table)
        ((expression-to-action e) e table)))))
                                               


