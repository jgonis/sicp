(include "littleSchemerCh7.sld")
(define-library (little-schemer tests ch7)
  (export run-tests-ch7)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch7))
  (begin
    (define run-tests-ch7
      (lambda ()
        (test-set?)
        (test-make-set)
        (test-alt-make-set)
        (test-subset?)
        (test-eqset?)
        (test-intersect?)
        (test-intersect)
        (test-union)
        (test-intersect-all)
        (test-a-pair?)
        (test-relation?)
        (test-fun?)
        (test-revrel)
        (test-family-fully-fun? fully-fun?)
        (test-family-fully-fun? alt-fully-fun?)
        (test-family-fully-fun? one-to-one?)))
    (define test-set?
      (lambda ()
        (check (set? '(apple peaches apple plum))
               => #f)
        (check (set? '())
               => #t)
        (check (set? '(apples peaches pears plums))
               => #t)
        (check (set? '(apple 3 pear 4 9 apple 3 4))
               => #f)
        (check (set? '(3 apple pear 4 9 3 4))
               => #f)))
    (define test-make-set
      (lambda ()
        (check (make-set '(apple
                           peach
                           pear
                           peach
                           plum
                           apple
                           lemon
                           peach))
               => '(pear plum apple lemon peach))
        (check (make-set '())
               => '())))
    (define test-alt-make-set
      (lambda ()
        (check (alt-make-set '())
               => '())
        (check (alt-make-set '(apple))
               => '(apple))
        (check (alt-make-set '(apple apple))
               => '(apple))
        (check (alt-make-set '(apple pear))
               => '(apple pear))
        (check (alt-make-set '(apple
                               peach
                               pear
                               peach
                               plum
                               apple
                               lemon
                               peach))
               => '(apple peach pear plum lemon))
        (check (alt-make-set '(apple
                               3
                               pear
                               4
                               9
                               apple
                               3
                               4))
               => '(apple 3 pear 4 9))))
    (define test-subset?
      (lambda ()
        (check (subset? '(5 chicken wings)
                        '(5 hamburgers
                            2 pieces fried chicken and
                            light duckling wings))
               => #t)
        (check (subset? '(4 pounds of horseradish)
                        '(four pounds chicken and
                               5 ounces horseradish))
               => #f)))
    (define test-eqset?
      (lambda ()
        (check (eqset? '(6 large chickens with wings)
                       '(6 chickens with large wings))
               => #t)
        (check (eqset? '()
                       '(6 large chickens with wings))
               => #f)
        (check (eqset? '(6 large chickens with wings)
                       '())
               => #f)
        (check (eqset? '(6 large chickens with wings)
                       '(6 chickens with wings))
               => #f)
        (check (eqset? '(6 chicken with wings)
                       '(6 large chickens with wings))
               => #f)))
    (define test-intersect?
      (lambda ()
        (check (intersect? '(stewed tomatoes and macaroni)
                           '(macaroni and cheese))
               => #t)
        (check (intersect? '()
                           '(macaroni and cheese))
               => #f)
        (check (intersect? '(stewed tomatoes and macaroni)
                           '(or cheese))
               => #f)
        (check (intersect? '(stewed tomatoes and macaroni)
                           '())
               => #f)))
    (define test-intersect
      (lambda ()
        (check (intersect '(stewed tomatoes and macaroni)
                          '(macaroni and cheese))
               => '(and macaroni))
        (check (intersect '()
                          '(macaroni and cheese))
               => '())
        (check (intersect '(stewed tomatoes and macaroni)
                          '())
               => '())
        (check (intersect '(stewed tomatoes and macaroni)
                          '(penne or cheese))
               => '())))
    (define test-union
      (lambda ()
        (check (union '(stewed tomatoes and macaroni casserole)
        '(macaroni and cheese))
               => '(stewed
                    tomatoes
                    and
                    macaroni
                    casserole
                    cheese))
        (check (union '()
                      '(macaroni and cheese))
               => '(macaroni and cheese))
        (check (union '(stewed tomatoes and macaroni casserole)
                      '())
               => '(stewed tomatoes and macaroni casserole))))
    (define test-intersect-all
      (lambda ()
        (check (intersect-all '((a b c)
                                (c a d e)
                                (e f g h a b)))
               => '(a))
        (check (intersect-all '((6 pears and)
                                (3 peaches and 6 peppers)
                                (8 pears and 6 plums)
                                (and 6 prunes with some apples)))
               => '(6 and))))
    (define test-a-pair?
      (lambda ()
        (check (a-pair? '(pear pear)) => #t)
        (check (a-pair? '(3 7)) => #t)
        (check (a-pair? '((2) (pair))) => #t)
        (check (a-pair? '(full (house))) => #t)))
    (define test-all-pairs?
      (lambda ()
        (check (all-pairs? '((1 2) (3 4) (5 6)))
               => #t)
        (check (all-pairs '((1 2) 3 (4 5) (6 7)))
               => #f)))
    (define test-relation?
      (lambda ()
        (check (relation? '(apples peaches pumpkin pie))
               => #f)
        (check (relation? '((apples peaches)
                            (pumpkin pie)
                            (apples peaches)))
               => #f)
        (check (relation? '((apples peaches)
                            (pumpkin pie)))
               => #t)
        (check (relation? '((4 3) (4 2) (7 6) (6 2) (3 4)))
               => #t)))
    (define test-fun?
      (lambda ()
        (check (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
               => #t)
        (check (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
               => #f)))
    (define test-revrel
      (lambda ()
        (check (revrel '((8 a) (pumpkin pie) (got sick)))
               => '((a 8) (pie pumpkin) (sick got)))
        (check (revrel '()) => '())))
    (define test-family-fully-fun?
      (lambda (fully-fun-func)
        (check (fully-fun-func '((8 3)
                                 (4 2)
                                 (7 6)
                                 (6 2)
                                 (3 4)))
               => #f)
        (check (fully-fun-func '((8 3)
                                 (4 8)
                                 (7 6)
                                 (6 2)
                                 (3 4)))
               => #t)
        (check (fully-fun-func '((grape raisin)
                                 (plum prune)
                                 (stewed prune)))
               => #f)
        (check (fully-fun-func '((grape raisin)
                                 (plum prune)
                                 (stewed grape)))
               => #t)))))
