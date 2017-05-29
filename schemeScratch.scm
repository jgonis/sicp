(define file-name "./testinputs/test.txt")
(define ip (open-input-file file-name))
(define (read-file input-port)
  (define (helper input-port q)
    (let ((next-char (read-char input-port)))
      (cond ((eof-object? next-char)
             (close-input-port input-port)
             q)
            (else
             (queue-push! q next-char)
             (helper input-port q)))))
  (helper input-port (make-queue)))

(define (create-input-set q ms)
  (cond ((queue-empty? q) ms)
        (else (let ((item (queue-peek q)))
                (queue-pop! q)
                (create-input-set q
                                  (adjoin-set item ms (lambda (x y) (char<? x y))))))))
(define (sort-largest->smallest lyst)
  (define (helper lyst item-to-insert)
    (cond ((null? lyst) (cons item-to-insert '()))
          ((> (cdr item-to-insert) (cdr (car lyst)))
           (cons item-to-insert lyst))
          (else (cons (car lyst) (helper (cdr lyst) item-to-insert)))))
  (define (helper2 lyst1 lyst2)
    (cond ((null? lyst1) lyst2)
          (else (helper2 (cdr lyst1) (helper lyst2 (car lyst1))))))
  (helper2 lyst '()))

(define (setup)
  (let* ((in-file (open-input-file file-name))
         (q (read-file in-file))
         (ms (create-input-set q (create-multiset '())))
         (ls (tree->list ms2))
         (sorted-list (sort-largest->smallest ls)))
    sorted-list))
