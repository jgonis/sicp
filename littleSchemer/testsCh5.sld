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
        (test-subst*)))
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
                    (plum brandy)))))))
