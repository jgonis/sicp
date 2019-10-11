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
        (test-insertR*)))
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
                    could chuck roast wood))))))
