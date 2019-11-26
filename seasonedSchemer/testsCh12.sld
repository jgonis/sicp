(include "seasonedSchemerCh12.sld")
(define-library (seasoned-schemer tests ch12)
  (export run-tests-ch12)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch12))
  (begin
    (define run-tests-ch12
      (lambda ()
        (test-multirember)
        (test-multirember-f)
        (test-lr-member?)
        (test-lr-union)))
    (define test-multirember
      (lambda ()
        (check (multirember 'tuna '(shrimp
                                    salad
                                    tuna
                                    salad
                                    and
                                    tuna))
               => '(shrimp salad salad and))
        (check (multirember 'tuna '())
               => '())
        (check (multirember 'tuna '(shrimp
                                    salad
                                    crab
                                    salad
                                    and
                                    crab))
               => '(shrimp salad crab salad and crab))
        (check (multirember 'tuna '(tuna tuna tuna))
               => '())
        (check (multirember 'pie '(apple
                                   custard
                                   pie
                                   linzer
                                   pie
                                   torte))
               => '(apple custard linzer torte))))
    (define test-multirember-f
      (lambda ()
        (let ((multirember-eq? (multirember-f eq?)))
          (check (multirember-eq? 'tuna '(shrimp
                                          salad
                                          tuna
                                          salad
                                          and
                                          tuna))
                 => '(shrimp salad salad and))
          (check (multirember-eq? 'tuna '())
                 => '())
          (check (multirember-eq? 'tuna '(shrimp
                                          salad
                                          crab
                                          salad
                                          and
                                          crab))
                 => '(shrimp salad crab salad and crab))
          (check (multirember-eq? 'tuna '(tuna tuna tuna))
                 => '())
          (check (multirember-eq? 'pie '(apple
                                         custard
                                         pie
                                         linzer
                                         pie
                                         torte))
                 => '(apple custard linzer torte)))))
    (define test-lr-member?
      (lambda ()
        (check (lr-member? 'ice '(salad
                                  greens
                                  with
                                  pears
                                  brie
                                  cheese
                                  frozen
                                  yogurt))
               => #f)
        (check (lr-member? 'ice '())
               => #f)
        (check (lr-member? 'ice '(ice))
               => #t)))
    (define test-lr-union
      (lambda ()
        (check (lr-union '() '())
               => '())
        (check (lr-union '() '(tomatoes and macaroni))
               => '(tomatoes and macaroni))
        (check (lr-union '(tomatoes and macaroni) '())
               => '(tomatoes and macaroni))
        (check (lr-union '(tomatoes and macaroni casserole)
                         '(macaroni and cheese))
               => '(tomatoes casserole macaroni and cheese))))))
