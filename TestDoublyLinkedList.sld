(include "DoublyLinkedList.sld")
(define-library (sicp test-dlists)
  (export test-all)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (sicp doubly-linked-list))
  (begin
    (define (test-all)
      (check-set-mode! 'summary)
      (let* ((test-subject (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 1 test-subject)))
        (check (element-count 1 test-subject) => 1)
        (check (element-count 1 test-subject2) => 2))
      (check-report)
      (check-reset!)
      (display ""))))
