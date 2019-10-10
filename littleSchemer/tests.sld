(include "littleSchemer.sld")
(define-library (little-schemer tests)
  (export run-tests)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer))
  (begin
    (define run-tests
      (lambda ()
        (check-reset!)
        (test-atom?)
        (test-lat?)
        (test-member?)
        (test-rember)
        (test-firsts)
        (test-insertR)
        (test-insertL)
        (test-subst)
        (test-subst2)
        (test-multirember)
        (test-multiinsertR)
        (test-multiinsertL)
        (test-multisubst)
        (test-j+)
        (test-j-)
        (test-addtup)
        (test-j*)
        (test-tup+)
        (test-j>)
        (test-j<)
        (test-j=)
        (test-alt-j=)
        (test-^)
        (test-j/)
        (test-j-length)
        (test-pick)
        (test-rempick)
        (test-no-nums)
        (test-all-nums)
        (test-occurs)
        (check-report)
        (check-reset!)))
    (define test-atom?
      (lambda ()
        (check (atom? (quote ())) => #f)))
    (define test-lat?
      (lambda ()
        (check (lat? '(Jack Sprat could eat no chicken fat)) => #t)
        (check (lat? '((Jack) Sprat could eat no chicken fat)) => #f)
        (check (lat? '(Jack (Sprat could) eat no chicken fat)) => #f)
        (check (lat? '()) => #t)))
    (define test-member?
      (lambda ()
        (check (member? 'tea
                        '(coffee tea or milk))
               => #t)
        (check (member? 'poached
                       '(fried eggs and scrambled eggs))
               => #f)
        (check (member? 'meat
                        '(mashed potatoes and meat gravy))
               => #t)))
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
                            '(lamb chops and mint flavored mint jelly))
               => '(lamb chops and mint pear flavored mint pear jelly))
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
                            '(lamb chops and mint flavored mint jelly))
               => '(lamb chops and pear mint flavored pear mint jelly))
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
                           '(lamb chops and mint flavored mint jelly))
               => '(lamb chops and pear flavored pear jelly))
        (check (multisubst 'egg
                           'toast
                           '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (multisubst 'lid
                           'cup
                           '(coffee cup tea cup and hick cup))
               => '(coffee lid tea lid and hick lid))))
    (define test-j+
      (lambda ()
        (check (j+ 46 12) => 58)))
    (define test-j-
      (lambda ()
        (check (j- 14 3) => 11)
        (check (j- 17 9) => 8)))
    (define test-addtup
      (lambda ()
        (check (addtup '(3 5 2 8)) => 18)
        (check (addtup '(15 6 7 12 3)) => 43)))
    (define test-j*
      (lambda ()
        (check (j* 5 3) => 15)
        (check (j* 13 4) => 52)))
    (define test-tup+
      (lambda ()
        (check (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
               => '(11 11 11 11 11))
        (check (tup+ '(2 3) '(4 6)) => '(6 9))
        (check (tup+ '(3 7) '(4 6)) => '(7 13))
        (check (tup+ '(3 7) '(4 6 8 1)) => '(7 13 8 1))
        (check (tup+ '(4 6 8 1) '(3 7)) => '(7 13 8 1))))
    (define test-j>
      (lambda ()
        (check (j> 12 133) => #f)
        (check (j> 120 11) => #t)
        (check (j> 3 3) => #f)))
    (define test-j<
      (lambda ()
        (check (j< 12 133) => #t)
        (check (j< 120 11) => #f)
        (check (j< 3 3) => #f)))
    (define test-j=
      (lambda ()
        (check (j= 4 3) => #f)
        (check (j= 3 4) => #f)
        (check (j= 3 3) => #t)))
    (define test-alt-j=
      (lambda ()
        (check (alt-j= 4 3) => #f)
        (check (alt-j= 3 4) => #f)
        (check (alt-j= 3 3) => #t)))
    (define test-^
      (lambda ()
        (check (^ 1 1) => 1)
        (check (^ 2 3) => 8)
        (check (^ 5 3) => 125)))
    (define test-j/
      (lambda ()
        (check (j/ 15 4) => 3)
        (check (j/ 3 6) => 0)))
    (define test-j-length
      (lambda ()
        (check (j-length '(hotdogs
                           with
                           mustard
                           sauerkraut
                           and
                           pickles))
               => 6)
        (check (j-length '(ham and cheese on rye)) => 5)))
    (define test-pick
      (lambda ()
        (check (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
               => 'macaroni)
        (check (pick 1 '(lasagna spaghetti ravioli macaroni meatball))
               => 'lasagna)))
    (define test-rempick
      (lambda ()
        (check (rempick 3 '(hotdogs with hot mustard))
               => '(hotdogs with mustard))))
    (define test-no-nums
      (lambda ()
        (check (no-nums '(5 pears 6 prunes 9 dates))
               => '(pears prunes dates))
        (check (no-nums '(1 2 3 4 5)) => '())))
    (define test-all-nums
      (lambda ()
        (check (all-nums '(5 pears 6 prunes 9 dates))
               => '(5 6 9))
        (check (all-nums '(pear prunes dates))
               => '())))
    (define test-occurs
      (lambda ()
        (check (occurs 'mint
                       '(lamb chops and mint flavored mint jelly))
               => 2)
        (check (occurs 'toast
                       '(bacon lettuce and tomato))
               => 0)))))
