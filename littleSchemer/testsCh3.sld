(include "littleSchemerCh3.sld")
(define-library (little-schemer tests ch3)
  (export run-tests-ch3)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch3))
  (begin
    (define run-tests-ch3
      (lambda ()
        (test-rember)
        (test-firsts)
        (test-insertR)
        (test-insertL)
        (test-subst)
        (test-subst2)
        (test-multirember)
        (test-multiinsertR)
        (test-multiinsertL)
        (test-multisubst)))
    (define test-rember
      (lambda ()
        (check (rember 'mint '(lamb chops and mint jelly))
               => '(lamb chops and jelly))
        (check (rember 'mint
                       '(lamb chops and mint flavored mint jelly))
               => '(lamb chops and flavored mint jelly))
        (check (rember 'toast
                       '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (rember 'cup
                       '(coffee cup tea cup and hick cup))
               => '(coffee tea cup and hick cup))))
    (define test-firsts
      (lambda ()
        (check (firsts '((apple peach pumpkin)
                         (plum pear cherry)
                         (grape raisin pea)
                         (bean carrot eggplant)))
               => '(apple plum grape bean))
        (check (firsts '((a b)
                         (c d)
                         (e f)))
               => '(a c e))
        (check (firsts '()) => '())
        (check (firsts '((five plums)
                         (four)
                         (eleven green oranges)))
               => '(five four eleven))
        (check (firsts '(((five plums) four)
                         (eleven green oranges)
                         ((no) more)))
               => '((five plums) eleven (no)))))
    (define test-insertR
      (lambda ()
        (check (insertR 'topping
                        'fudge
                        '(ice cream with fudge for dessert))
               => '(ice cream with fudge topping for dessert))
        (check (insertR 'jalapeno
                        'and
                        '(tacos tamales and salsa))
               => '(tacos tamales and jalapeno salsa))
        (check (insertR 'e 'd '(a b c d f g h))
               => '(a b c d e f g h))))
    (define test-insertL
      (lambda ()
        (check (insertL 'topping
                        'fudge
                        '(ice cream with fudge for dessert))
               => '(ice cream with topping fudge for dessert))
        (check (insertL 'jalapeno
                        'and
                        '(tacos tamales and salsa))
               => '(tacos tamales jalapeno and salsa))
        (check (insertL 'e 'd '(a b c d f g h))
               => '(a b c e d f g h))))
    (define test-subst
      (lambda ()
        (check (subst 'topping
                      'fudge
                      '(ice cream with fudge for dessert))
               => '(ice cream with topping for dessert))))
    (define test-subst2
      (lambda ()
        (check (subst2 'vanilla
                       'chocolate
                       'banana
                       '(banana ice cream with chocolate topping))
               => '(vanilla ice cream with chocolate topping))))
    (define test-multirember
      (lambda ()
        (check (multirember 'mint
                       '(lamb chops and mint flavored mint jelly))
               => '(lamb chops and flavored jelly))
        (check (multirember 'toast
                       '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (multirember 'cup
                       '(coffee cup tea cup and hick cup))
               => '(coffee tea and hick))))
    (define test-multiinsertR
      (lambda ()
        (check (multiinsertR 'pear
                             'mint
                            '(lamb
                              chops
                              and
                              mint
                              flavored
                              mint
                              jelly))
               => '(lamb
                    chops
                    and
                    mint
                    pear
                    flavored
                    mint
                    pear
                    jelly))
        (check (multiinsertR 'egg
                             'toast
                            '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (multiinsertR 'lid
                             'cup
                            '(coffee cup tea cup and hick cup))
               => '(coffee cup lid tea cup lid and hick cup lid))))
    (define test-multiinsertL
      (lambda ()
        (check (multiinsertL 'pear
                             'mint
                            '(lamb
                              chops
                              and
                              mint
                              flavored
                              mint
                              jelly))
               => '(lamb
                    chops
                    and
                    pear
                    mint
                    flavored
                    pear
                    mint
                    jelly))
        (check (multiinsertL 'egg
                             'toast
                            '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (multiinsertL 'lid
                             'cup
                            '(coffee cup tea cup and hick cup))
               => '(coffee lid cup tea lid cup and hick lid cup))))
    (define test-multisubst
      (lambda ()
        (check (multisubst 'pear
                           'mint
                           '(lamb
                             chops
                             and
                             mint
                             flavored
                             mint
                             jelly))
               => '(lamb chops and pear flavored pear jelly))
        (check (multisubst 'egg
                           'toast
                           '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (multisubst 'lid
                           'cup
                           '(coffee cup tea cup and hick cup))
               => '(coffee lid tea lid and hick lid))))))
