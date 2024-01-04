(define-library (seasoned-schemer ch10)
  (export displayln
          new-entry
          lookup-in-entry
          empty-table
          extend-table
          combine-tables
          lookup-in-table
          expression-to-type
          type-to-action
          value
          meaning
          const-action
          quote-action
          cond-action
          eval-cond
          question-of
          answer-of
          else?)
  (import (scheme base)
          (scheme write)
          (scheme list)
          (little-schemer ch1)
          (seasoned-schemer ch11))
  (begin
    (define displayln
      (lambda (indent-level . outputs)
        1
        ;; (let indent-loop ((current-indent-level indent-level))
        ;;   (cond ((> current-indent-level 0) (begin (display "\t")
        ;;                                            (indent-loop (- current-indent-level 1))))))
        ;; (let loop ((output-values outputs))
        ;;   (cond ((null? output-values) (newline))
        ;;         (else (begin (display (car output-values))
        ;;                      (loop (cdr output-values))))))
        ))    
    (define new-entry
      (lambda (name-set value-set)
        (cons name-set (cons value-set (quote ())))))
    
    (define lookup-in-entry
      (lambda  (name entry not-found-f)
        (letrec ((helper (lambda (name names values)
                           (cond ((null? names) (not-found-f name))
                                 ((equal? name (first names)) (first values))
                                 (else (helper name (cdr names) (cdr values)))))))
          (cond ((null? entry) (not-found-f name))
                (else (helper name (first entry) (second entry)))))))

    (define empty-table '())

    (define extend-table
      (lambda (new-entry old-table)
        (cons new-entry old-table)))

    (define combine-tables
      (lambda (new-table base-table)
        (let loop ((source-table new-table))
          (cond ((null? source-table) base-table)
                (else (cons (car source-table)
                            (loop (cdr source-table))))))))

    (define lookup-in-table
      (lambda (name table name-not-found-f)
        (cond ((null? table) (name-not-found-f name))
              (else (lookup-in-entry name
                                     (car table)
                                     (lambda (name)
                                       (lookup-in-table name
                                                        (cdr table)
                                                        name-not-found-f)))))))

    ;; We are using *const to signify "primitive" objects such as #f, 6, and
    ;; cons.
    ;; Primitive atoms that aren't functions return themselves when evaluated.
    ;; Primitive functions return (primitive func-name) when evaluated.

    (define value
      (lambda (expression)
        (meaning expression empty-table 0)))
    (define meaning
      (lambda (expression table indent-level)
        (displayln indent-level "meaning func, expression: " expression " table: " table)
        ((type-to-action (expression-to-type expression indent-level)) expression table (+ indent-level 1))))

    (define expression-to-type
      (lambda (expression indent-level)
        (cond ((atom? expression) (atom-to-type expression))
              (else (list-to-type expression indent-level)))))
    (define atom-to-type
      (lambda (atom-expression)
        (cond ((number? atom-expression) '*const)
              ((eq? atom-expression #t) '*const)
              ((eq? atom-expression #f) '*const)
              ((eq? atom-expression 'cons) '*const)
              ((eq? atom-expression 'car) '*const)
              ((eq? atom-expression 'cdr) '*const)
              ((eq? atom-expression 'null?) '*const)
              ((eq? atom-expression 'eq?) '*const)
              ((eq? atom-expression 'atom?) '*const)
              ((eq? atom-expression 'zero?) '*const)
              ((eq? atom-expression '+) '*const)
              ((eq? atom-expression '-) '*const)
              ((eq? atom-expression 'number?) '*const)
              (else '*identifier))))

    (define list-to-type
      (lambda (list-expression indent-level)
        (displayln indent-level "list-to-type list-expression: " list-expression)
        (let ((first-element (first list-expression)))
          (cond ((eq? first-element 'quote) '*quote)
                ((eq? first-element 'lambda) '*lambda)
                ((eq? first-element 'cond) '*cond)
                (else '*application)))))
    

    (define type-to-action
      (lambda (type)
        (cond ((eq? type '*const) const-action)
              ((eq? type '*identifier) identifier-action)
              ((eq? type '*quote) quote-action)
              ((eq? type '*lambda) lambda-action)
              ((eq? type '*cond) cond-action)
              (else application-action))))

    (define const-action
      (lambda (expression table indent-level)
        (displayln indent-level "const-action, expression: " expression " table: " table)
        (cond ((number? expression) expression)
              ((eq? #t expression) #t)
              ((eq? #f expression) #f)
              (else (cons 'primitive (cons expression '()))))))
    
    (define identifier-action
      (lambda (expression table indent-level)
        (displayln indent-level "identifier-action, expression: " expression " table: " table)
        (lookup-in-table expression
                         table
                         (lambda (name) (error "could name find name in environment" name table)))))
    (define quote-action
      (lambda (expression table indent-level)
        (displayln indent-level "quote-action, expression: " expression " table: " table)
        (second expression)))
    
    (define cond-action
      (lambda (expression table indent-level)
        (displayln indent-level "cond-action, expression: " expression " table: " table)
        (let ((get-cond-clauses-of-expression cdr))
          (eval-cond (get-cond-clauses-of-expression expression) table indent-level))))
    (define eval-cond
      (lambda (lines table indent-level)
        (let ((first-clause (first lines)))
          (cond ((else? (question-of first-clause)) (meaning (answer-of first-clause) table (+ indent-level 1)))
                ((meaning (question-of first-clause) table (+ indent-level 1)) (meaning (answer-of first-clause) table (+ indent-level 1)))
                ((not (null? (cdr lines))) (eval-cond (cdr lines) table indent-level))))))
    (define question-of
      (lambda (cond-clause)
        (first cond-clause)))
    (define answer-of
      (lambda (cond-clause)
        (second cond-clause)))
    (define else?
      (lambda (question-clause)
        (eq? question-clause 'else)))

    
    (define lambda-action
      (lambda (expression table indent-level)
        (displayln indent-level "lambda-action, expression: " expression " table: " table)
        (cons 'non-primitive
              (cons (cons table (cdr expression)) '()))))
    
    
    (define application-action
      (lambda (expression table indent-level)
        (displayln indent-level "application-action, expression: " expression " table: " table)
        (my-apply (meaning (function-of expression) table (+ indent-level 1))
                  (evaluate-arg-list (arguments-of expression) table (+ indent-level 1))
                  table
                  (+ indent-level 1))))
    (define my-apply
      (lambda (function-representation
          argument-values
          table
          indent-level)
        (displayln indent-level "my-apply, function-rep: " function-representation " arg-values: " argument-values)
        (cond ((primitive? function-representation) (apply-primitive (second function-representation) argument-values indent-level))
              ((non-primitive? function-representation) (apply-closure (second function-representation)
                                                                       argument-values
                                                                       table
                                                                       indent-level)))))
    (define apply-primitive
      (lambda (primitive-func argument-values indent-level)
        (displayln indent-level "apply-primitive, func: " primitive-func " arg-vals: " argument-values)
        (cond ((eq? primitive-func 'cons) (cons (first argument-values) (second argument-values)))
              ((eq? primitive-func 'car) (car (first argument-values)))
              ((eq? primitive-func 'cdr) (cdr (first argument-values)))
              ((eq? primitive-func 'null?) (null? (first argument-values)))
              ((eq? primitive-func 'eq?) (eq? (first argument-values) (second argument-values)))
              ((eq? primitive-func 'atom?) (my-atom? (first argument-values)))
              ((eq? primitive-func 'zero?) (zero? (first argument-values)))
              ((eq? primitive-func '+) (+ (first argument-values) (second argument-values)))
              ((eq? primitive-func '-) (- (first argument-values) (second argument-values)))
              ((eq? primitive-func 'number?) (number? (first argument-values))))))
    (define my-atom?
      (lambda (value)
        (cond ((atom? value) #t)
              ((null? value) #f)
              ((eq? (first value) 'primitive) #t)
              ((eq? (first value) 'non-primitive) #t)
              (else #f))))
    (define apply-closure
      (lambda (closure-table arg-values table indent-level)
        (displayln indent-level "apply-closure, closure-table: " closure-table " arg-vals: " arg-values)
        (meaning (body-of closure-table)
                 ;; This allows for functions to recursively refer to themselves, but I am not sure of the
                 ;; other consequences of it. I am not sure about the scope impliciations by values
                 ;; are looked up in order, so we first look at arg literals, then the table at the time
                 ;; the function was created, and finally the table that exists at the time of the function's
                 ;; execution. Is this lexical?
                 ;; (combine-tables (extend-table (new-entry (formals-of closure-table) arg-values)
                 ;;                               (table-of closure-table))
                 ;;                 table)
                 ;;By passing in only the function argument values and the table
                 ;;that existed at the time of the function's creation we can't use
                 ;;recursive functions that mention themselves or anything created after them
                 (extend-table (new-entry (formals-of closure-table) arg-values)
                               (table-of closure-table))
                 (+ indent-level 1))))
    (define table-of
      (lambda (closure-representation)
        (first closure-representation)))
    (define formals-of
      (lambda (closure-representation)
        (second closure-representation)))
    (define body-of
      (lambda (closure-representation)
        (third closure-representation)))
    
    (define function-of
      (lambda (function-expression)
        (first function-expression)))
    (define arguments-of
      (lambda (function-expression)
        (cdr function-expression)))
    (define primitive?
      (lambda (closure-record)
        (eq? (first closure-record) 'primitive)))
    (define non-primitive?
      (lambda (closure-record)
        (eq? (first closure-record) 'non-primitive)))
    (define evaluate-arg-list
      (lambda (arg-list table indent-level)
        (displayln indent-level "evaluate-arg-list: " arg-list)
        (cond ((null? arg-list) '())
              (else (cons (meaning (first arg-list) table (+ indent-level 1))
                          (evaluate-arg-list (cdr arg-list) table (+ indent-level 1)))))))
    
    ;; How does extend work? It is a function that receives a name
    ;; a value and a table, and returns a function that receives another
    ;; name. If the name passed to the returned function is the same as
    ;; the name used when extend was first called, we return the value
    ;; that was passed in when it was called. Otherwise we apply table
    ;; to the name passed in to the returned function. In this way you
    ;; can keep adding on the values associated with a name in a table.
    ;; Each time you call extend you get a new table with a name associated
    ;; with a value. If this name passed to this new table is not the one it
    ;; has associated with a value, then it recursively delegates to the
    ;; table it has inside to see if that table has a value associated with
    ;; the name and so on, until it reaches the "base" table, or empty table
    (define extend
      (lambda (name1 value table)
        (lambda (name2)
          (cond ((eq? name1 name2) value)
                (else (table name2))))))))


