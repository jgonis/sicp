(define-library (seasoned-schemer ch20)
  (export jeff-ch20)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define the-empty-table
      (lambda (name)
        ))

    (define lookup
      (lambda (table name)
        (table name)))

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
                (else (table name2))))))

    (define value
      (lambda (expression)
        ))))
