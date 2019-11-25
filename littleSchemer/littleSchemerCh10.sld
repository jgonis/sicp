(include "littleSchemerCh1.sld")
(include "littleSchemerCh4.sld")
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
          lines-of-cond
          *const
          *identifier
          *cond
          *lambda
          *application
          *quote)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
           (little-schemer ch4)
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
    (define value
      (lambda (e)
        (meaning e '())))
    (define meaning
      (lambda (e table)
        ((expression-to-action e) e table)))
    (define expression-to-action
      (lambda (e)
        (cond ((atom? e) (atom-to-action e))
              (else (list-to-action e)))))
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
               (cond ((eq? (car l) 'quote) *quote)
                     ((eq? (car l) 'lambda) *lambda)
                     ((eq? (car l) 'cond) *cond)
                     ((eq? (car l) 'define) *define)
                     (else *application)))
              (else *application))))
    (define *const
      (lambda (expr table)
        (cond ((number? expr) expr)
              ((eq? expr #t) #t)
              ((eq? expr #f) #f)
              (else (build-pair 'primitive expr)))))
    (define *quote
      (lambda (expr table)
        (second expr)))
    (define *identifier
      (lambda (expr table)
        (lookup-in-table expr
                         table
                         (lambda (name)
                           (error "Can't find identifier"
                                  (quote name))))))
    (define *lambda
      (lambda (expr table)
        (build-pair 'non-primitive
                    (cons table (cdr expr)))))
    (define table-of
      (lambda (func)
        (first func)))
    (define formal-arguments-of
      (lambda (func)
        (second func)))
    (define body-of-func
      (lambda (func)
        (third func)))
    (define *cond
      (lambda (expr table)
        (evaluate-cond (lines-of-cond expr) table)))
    (define lines-of-cond
      (lambda (expr)
        (cdr expr)))
    (define evaluate-cond
      (lambda (lines table)
        (cond ((null? lines) (error "nothing true, no else?"))
              ((else? (question-of (car lines)))
               (meaning (answer-of (car lines))
                        table))
              ((meaning (question-of (car lines)) table)
               (meaning (answer-of (car lines)) table))
              (else (evaluate-cond (cdr lines) table)))))
    (define else?
      (lambda (cond-question)
        (eq? cond-question 'else)))
    (define question-of
      (lambda (cond-line)
        (first cond-line)))
    (define answer-of
      (lambda (cond-line)
        (second cond-line)))
    (define *application
      (lambda (expr table))
      (my-apply (meaning (function-of expr) table)
                (evlist (arguments-of expr) table)))
    (define function-of
      (lambda (expr)
        (first expr)))
    (define arguments-of
      (lambda (expr)
        (cdr expr)))
    (define evlist
      (lambda (list-of-expressions table)
        (cond ((null? list-of-expressions) '())
              (else (cons (meaning (car list-of-expressions)
                                   table)
                          (evlist (cdr list-of-expressions)
                                  table))))))
    (define primitive?
      (lambda (l)
        (eq? (car l) 'primitive)))
    (define non-primitive?
      (lambda (l)
        (eq? (car l) 'non-primitive)))
    (define my-apply
      (lambda (func arg-values)
        (cond ((primitive? func)
               (apply-primitive (second func)
                                arg-values))
              ((non-primitive? func)
               (apply-closure (second func)
                              arg-values))
              (else (error "Unknown application"
                           (build-pair func args))))))
    (define apply-primitive
      (lambda (name vals)
        (cond ((eq? name 'cons)
               (if (not (= (length vals) 2))
                   (error "Expected 2 args for cons" vals)
                   (cons (first vals) (second vals))))
              ((eq? name 'car)
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for car" vals))
               (if (atom? (first vals))
                   (error "Expected a list arg for car" vals))
               (car (first vals)))
              ((eq? name 'cdr)
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for cdr" vals))
               (if (atom? (first vals))
                   (error "Expected a list arg for cdr" vals))
               (cdr (first vals)))
              ((eq? name 'null?)
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for null?" vals))
               (if (atom? (first vals))
                   (error "Expected a list arg for null?" vals))
               (null? (first vals)))
              ((eq? name 'eq?)
               (if (not (= (length vals) 2))
                   (error "Expected 2 args for eq?" vals))
               (eq? (first vals) (second vals)))
              ((eq? name 'atom?)
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for atom?"))
               (cond ((atom? (first vals)) #t)
                     ((null? (first vals)) #f)
                     ((eq? (car (first vals)) 'primitive) #t)
                     ((eq? (car (first vals)) 'non-primitive) #t)
                     (else #f)))
              ((eq? name 'zero?)
               (if (not (number? (first vals)))
                   (error "Expected a number arg for zero?" vals))
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for zero?" vals))
               (zero? (first vals)))
              ((eq? name 'add1)
               (if (not (number? (first vals)))
                   (error "Expected a number arg for add1" vals))
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for add1" vals))
               (add1 (first vals)))
              ((eq? name 'sub1)
               (if (not (number? (first vals)))
                   (error "Expected a number arg for sub1" vals))
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for sub1" vals))
               (sub1 (first vals)))
              ((eq? name 'number?)
               (if (not (= (length vals) 1))
                   (error "Expected 1 arg for number?" vals))
               (number? (first vals)))
              (else (error "Unrecognized primitive func" name)))))
    (define apply-closure
      (lambda (closure vals)
        (meaning (body-of-func closure)
                 (extend-table (new-entry (formal-arguments-of closure)
                                          vals)
                               (table-of closure)))))))
                                               


