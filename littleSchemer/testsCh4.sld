(include "littleSchemerCh4.sld")
(define-library (little-schemer tests ch4)
  (export run-tests-ch4)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch4))
  (begin
    (define run-tests-ch4
      (lambda ()
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
        (test-one?)))
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
               => 0)))
    (define test-one?
      (lambda ()
        (check (one? 1) => #t)
        (check (one? 2) => #f)
        (check (one? 'j) => #f)
        (check (one? '()) => #f)))))
