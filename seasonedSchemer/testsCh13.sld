(include "seasonedSchemerCh13.sld")
(define-library (seasoned-schemer tests ch13)
  (export run-tests-ch13)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch13))
  (begin
    (define run-tests-ch13
      (lambda ()
        (test-lr-intersect)
        (test-lr-intersectall)
        (test-lr-rember)
        (test-rember-beyond-first)
        (test-rember-upto-last)))
    (define test-lr-intersect
      (lambda ()
        (check (lr-intersect '() '())
               => '())
        (check (lr-intersect '() '(tomatoes and macaroni))
               => '())
        (check (lr-intersect '(tomatoes and macaroni)
                             '(macaroni and cheese))
               => '(and macaroni))))
    (define test-lr-intersectall
      (lambda ()
        (check (lr-intersectall '((a b c)
                                  (c a d e)
                                  (e f g h a b)))
               => '(a))
        (check (lr-intersectall '((6 pears and)
                                  (3 peaches and 6 peppers)
                                  (8 pears and 6 plums)
                                  (and 6 prunes with some apples)))
               => '(6 and))
        (check (lr-intersectall '())
               => '())
        (check (lr-intersectall '((3 mangos and)
                                  (3 kiwis and)
                                  (3 hamburgers)))
               => '(3))
        (check (lr-intersectall '((3 steaks and)
                                  (no food and)
                                  (three baked potatoes)
                                  (3 diet hamburgers)))
               => '())
        (check (lr-intersectall '((3 mangoes and)
                                  ()
                                  (3 diet hamburgers)))
               => '())))
    (define test-lr-rember
      (lambda ()
        (check (lr-rember 'mint '(lamb chops and mint jelly))
               => '(lamb chops and jelly))
        (check (lr-rember 'mint
                          '(lamb
                            chops
                            and
                            mint
                            flavored
                            mint
                            jelly))
               => '(lamb chops and flavored mint jelly))
        (check (lr-rember 'toast
                          '(bacon lettuce and tomato))
               => '(bacon lettuce and tomato))
        (check (lr-rember 'cup
                          '(coffee cup tea cup and hick cup))
               => '(coffee tea cup and hick cup))))
    (define test-rember-beyond-first
      (lambda ()
        (let ((input1 '(noodles
                        spaghetti
                        spatzle
                        bean-thread
                        roots
                        potatoes
                        yam
                        others
                        rice)))
          (check (rember-beyond-first 'test '())
          => '())
          (check (rember-beyond-first 'rice input1)
          => '(noodles
               spaghetti
               spatzle
               bean-thread
               roots
               potatoes
               yam
               others))
          (check (rember-beyond-first 'roots input1)
          => '(noodles spaghetti spatzle bean-thread))
          (check (rember-beyond-first 'others input1)
          => '(noodles
               spaghetti
               spatzle
               bean-thread
               roots
               potatoes
               yam))
          (check (rember-beyond-first 'sweetthing input1)
                 => input1))))
    (define test-rember-upto-last
      (lambda ()
        (let ((input '(noodles
                       spaghetti
                       spatzle
                       bean-thread
                       roots
                       potatoes
                       yam
                       others
                       rice)))
          (check (rember-upto-last 'roots '())
                 => '())
          (check (rember-upto-last 'rice input)
                  => '())
          (check (rember-upto-last 'roots input)
                 => '(potatoes yam others rice))
          (check (rember-upto-last 'sweetthing input)
                 => input)
          (let ((desert-input '(cookies
                                chocolate mints
                                caramel delight ginger snaps
                                desserts
                                chocolate mousse
                                vanilla ice cream
                                German chocolate cake
                                more cookies
                                gingerbreadman chocolate
                                chip brownies)))
            (check (rember-upto-last 'cookies
                              desert-input)
                   => '(gingerbreadman
                        chocolate
                        chip
                        brownies))))))))
