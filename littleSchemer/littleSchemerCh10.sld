(include "littleSchemerCh1.sld")
(include "littleSchemerCh7.sld")
(define-library (little-schemer ch10)
  (export new-entry
          lookup-in-entry
          extend-table
          lookup-in-table
          expression-to-action
          atom-to-action
          list-to-action)
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
      (lambda (a) #f))
    (define list-to-action
      (lambda (l) #f))
    (define expression-to-action
      (lambda (e)
        (cond ((atom? e) (atom-to-action))
              (else (list-to-action e)))))))
                                               


