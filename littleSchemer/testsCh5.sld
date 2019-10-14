(include "littleSchemerCh5.sld")
(define-library (little-schemer tests ch5)
  (export run-tests-ch5)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch5))
  (begin
    (define run-tests-ch5
      (lambda ()
        (test-rember*)
        (test-insertR*)
        (test-occurs*)
        (test-subst*)
        (test-insertL*)
        (test-member*?)
        (test-eqlist?)))
    (define test-rember*
      (lambda ()
        (check (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
               => '((coffee) ((tea)) (and (hick))))
        (check (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
               => '(((tomato)) ((bean)) (and ((flying)))))))
    (define test-insertR*
      (lambda ()
        (check (insertR* 'roast
                         'chuck
                         '((how much (wood))
                           could
                           ((a (wood) chuck))
                           (((chuck)))
                           (if (a) ((wood chuck)))
                           could chuck wood))
               => '((how much (wood))
                    could
                    ((a (wood) chuck roast))
                    (((chuck roast)))
                    (if (a) ((wood chuck roast)))
                    could chuck roast wood))
        (check (insertR* 'roast
                         'chuck
                         '((how much (wood))
                           could
                           ((a (wood) squirrel))
                           (((throw)))
                           (if (a) ((wood squirrel)))
                           could throw wood))
               => '((how much (wood))
                    could
                    ((a (wood) squirrel))
                    (((throw)))
                    (if (a) ((wood squirrel)))
                    could throw wood))
        (check (insertR* 'roast
                         'chuck
                         '())
               => '())))
    (define test-occurs*
      (lambda ()
        (check (occurs* 'chuck
                        '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))
               => 4)
        (check (occurs* 'chuck
                        '())
               => 0)
        (check (occurs* 'chuck
                        '((how much (wood))
                           could
                           ((a (wood) squirrel))
                           (((throw)))
                           (if (a) ((wood squirrel)))
                           could throw wood))
               => 0)))
    (define test-subst*
      (lambda ()
        (check (subst* 'orange
                      'banana
                      '())
               => '())
        (check (subst* 'orange
                       'banana
                       '((banana)
                         (split ((((banana ice)))
                                 (cream (banana))
                                 sherbet))
                         (banana)
                         (bread)
                         (banana brandy)))
               => '((orange)
                    (split ((((orange ice)))
                            (cream (orange))
                            sherbet))
                    (orange)
                    (bread)
                    (orange brandy)))
        (check (subst* 'orange
                       'banana
                       '((plum)
                         (split ((((plum ice)))
                                 (cream (plum))
                                 sherbet))
                         (plum)
                         (bread)
                         (plum brandy)))
               => '((plum)
                    (split ((((plum ice)))
                            (cream (plum))
                            sherbet))
                    (plum)
                    (bread)
                    (plum brandy)))))
    (define test-insertL*
      (lambda ()
        (check (insertL* 'pecker
                         'chuck
                         '((how much (wood))
                           could
                           ((a (wood) chuck))
                           (((chuck)))
                           (if (a) ((wood chuck)))
                           could chuck wood))
               => '((how much (wood))
                    could
                    ((a (wood) pecker chuck))
                    (((pecker chuck)))
                    (if (a) ((wood pecker chuck)))
                    could pecker chuck wood))))
    (define test-member*?
      (lambda ()
        (check (member*? 'chips
                         '())
               => #f)
        (check (member*? 'chips
                         '((potato)
                           (crisps ((with) fish) (crisps))))
               => #f)
        (check (member*? 'chips
                         '((potato)
                           (chips ((with) fish) (chips))))
               => #t)))
    (define test-eqlist?
      (lambda ()
        (check (eqlist? '(strawberry ice cream)
                        '(strawberry ice cream))
               => #t)
        (check (eqlist? '(strawberry ice cream)
                        '(strawberry cream ice))
               => #f)
        (check (eqlist? '(banana ((split)))
                        '((banana) (split)))
               => #f)
        (check (eqlist? '(beef ((sausage)) (and (soda)))
                        '(beef ((salami)) (and (soda))))
               => #f)
        (check (eqlist? '(beef ((sausage)) (and (soda)))
                        '(beef ((sausage)) (and (soda))))
               => #t)))))
