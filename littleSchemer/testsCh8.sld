(include "littleSchemerCh8.sld")
(include "littleSchemerCh3.sld")
(define-library (little-schemer tests ch8)
  (export run-tests-ch8)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch8)
          (little-schemer ch3))
  (begin
    (define run-tests-ch8
      (lambda ()
        (test-rember-f)
        (test-rember-gen)
        (test-insertL-f eq?)
        (test-insertL-f equal?)
        (test-insertR-f eq?)
        (test-insertR-f equal?)
        (test-subst-f eq?)
        (test-subst-f equal?)
        (test-insert-gen insert-left insertL-f)
        (test-insert-gen insert-right insertR-f)
        (test-insert-gen insert-only-new subst-f)
        (test-insert-gen-rember insert-nothing)))
    (define test-rember-f
      (lambda ()
        (check (rember-f =
                         5
                         '(6 2 5 3))
               => '(6 2 3))
        (check (rember-f eq?
                         'jelly
                         '(jelly beans are good))
               => '(beans are good))
        (check (rember-f equal?
                         '(pop corn)
                         '(lemonade (pop corn) and (cake)))
               => '(lemonade and (cake)))))
    (define test-rember-gen
      (lambda ()
        (check ((rember-gen =)
                5
                '(6 2 5 3))
               => '(6 2 3))
        (check ((rember-gen eq?)
                'jelly
                '(jelly beans are good))
               => '(beans are good))
        (check ((rember-gen equal?)
                '(pop corn)
                '(lemonade (pop corn) and (cake)))
               => '(lemonade and (cake)))))
    (define test-insertL-f
      (lambda (test-func)
        (check ((insertL-f test-func)
                'topping
                'fudge
                '(ice cream with fudge for dessert))
               => '(ice cream with topping fudge for dessert))
        (check ((insertL-f test-func)
                'jalapeno
                'and
                '(tacos tamales and salsa))
               => '(tacos tamales jalapeno and salsa))
        (check ((insertL-f test-func)
                'e
                'd
                '(a b c d f g h))
               => '(a b c e d f g h))))
    (define test-insertR-f
      (lambda (test-func)
        (check ((insertR-f test-func)
                'topping
                'fudge
                '(ice cream with fudge for dessert))
               => '(ice cream with fudge topping for dessert))
        (check ((insertR-f test-func)
                'jalapeno
                'and
                '(tacos tamales and salsa))
               => '(tacos tamales and jalapeno salsa))
        (check ((insertR-f test-func)
                'e
                'd
                '(a b c d f g h))
               => '(a b c d e f g h))))
    (define test-subst-f
      (lambda (test-func)
        (check ((subst-f test-func)
                'topping
                'fudge
                '(ice cream with fudge for dessert))
               => '(ice cream with topping for dessert))
        (check ((subst-f test-func)
                'jalapeno
                'and
                '(tacos tamales and salsa))
               => '(tacos tamales jalapeno salsa))
        (check ((subst-f test-func)
                'e
                'd
                '(a b c d f g h))
               => '(a b c e f g h))))
    (define test-insert-gen
      (lambda (insert-func testing-func)
        (check (((insert-gen insert-func) equal?)
                'topping
                'fudge
                '(ice cream with fudge for dessert))
               => ((testing-func equal?)
                   'topping
                   'fudge
                   '(ice cream with fudge for dessert)))
        (check (((insert-gen insert-func) equal?)
                'jalapeno
                'and
                '(tacos tamales and salsa))
               => ((testing-func equal?)
                   'jalapeno
                   'and
                   '(tacos tamales and salsa)))
        (check (((insert-gen insert-func) equal?)
                'e
                'd
                '(a b c d f g h))
               => ((testing-func equal?)
                   'e
                   'd
                   '(a b c d f g h)))))
    (define test-insert-gen-rember
      (lambda (insert-func)
        (check (((insert-gen insert-func) =)
                #f
                5
                '(6 2 5 3))
               => (rember-f =
                            5
                            '(6 2 5 3)))
        (check (((insert-gen insert-func) eq?)
                #f
                'jelly
                '(jelly beans are good))
               => (rember-f eq?
                            'jelly
                            '(jelly beans are good)))
        (check (((insert-gen insert-func) equal?)
                #f
                '(pop corn)
                '(lemonade (pop corn) and (cake)))
               => (rember-f equal?
                            '(pop corn)
                            '(lemonade (pop corn) and (cake))))))))
