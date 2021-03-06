(include "Multisets.sld")
(include "tree.sld")
(include "sets.sld")

(define-library (sicp huffman-base)
  (export leaf?
          make-leaf
          tree-items
          tree-weight
          make-code-tree)
  (import (scheme base)
          (scheme cxr)
          (sicp tree-lib))
  (begin
    (define (leaf? tree)
      (and (null? (left-branch tree))
           (null? (right-branch tree))))
    (define (make-leaf item weight)
      (make-tree (cons (list item) weight) '() '()))
    (define (tree-items tree)
      (car (node tree)))
    (define (tree-weight tree)
      (cdr (node tree)))
    (define (make-code-tree left-tree right-tree)
      (make-tree (cons (append (tree-items left-tree)
                               (tree-items right-tree))
                       (+ (tree-weight left-tree)
                          (tree-weight right-tree)))
                 left-tree
                 right-tree))))

(define-library (sicp huffman-encoding)
  (export encode-item
          make-leaf-set
          encode)
  (import (scheme base)
          (sicp huffman-base)
          (sicp tree-lib)
          (sicp ordered-list-set))
  (begin    
    (define (make-leaf-set pairs)
      (make-set (map (lambda (pair) (make-leaf (car pair) (cdr pair)))
                     pairs)
                (lambda (a b) (< (tree-weight a) (tree-weight b)))))
    
    (define (encode message tree)
      (cond ((null? message) '())
            (else (append (encode-item (car message)
                                         tree)
                          (encode (cdr message)
                                  tree)))))
    
    (define (encode-item symbol tree)
      (define (go-left? symbol symbol-list)
        (cond ((null? symbol-list) #f)
              ((eq? symbol (car symbol-list)) #t)
              (else (go-left? symbol (cdr symbol-list)))))
      (cond ((leaf? tree) (cond ((eq? (car (tree-items tree)) symbol) '())
                                (else (error "unknown symbol!"))))
            ((go-left? symbol (tree-items (left-branch tree)))
             (cons 0 (encode-item symbol (left-branch tree))))
            (else (cons 1 (encode-item symbol (right-branch tree))))))))

(define-library (sicp huffman-decoding)
  (export decode)
  (import (scheme base)
          (sicp huffman-base)
          (sicp tree-lib))
  (begin
    (define (decode bits tree)
      (define (choose-branch bit tree)
        (cond ((= bit 0) (left-branch tree))
              ((= bit 1) (right-branch tree))
              (else (error "bad bit!"))))
      (define (decode-1 bits current-branch)
        (cond ((null? bits) '())
              (else (let ((next-branch (choose-branch (car bits)
                                                      current-branch)))
                      (cond ((leaf? next-branch)
                             (cons (car (tree-items next-branch))
                                   (decode-1 (cdr bits) tree)))
                            (else (decode-1 (cdr bits)
                                            next-branch)))))))
      (decode-1 bits tree))))


(define-library (sicp generate-huffman)
  (export generate-huffman-tree)
  (import (scheme base)
          (sicp huffman-encoding)
          (sicp huffman-base)
          (sicp ordered-list-set))
  (begin
    (define (generate-huffman-tree pairs)
      (define (successive-merge pairs)
        (cond ((= (length pairs) 1) (car pairs))
              (else (let ((first (car pairs))
                          (second (cadr pairs))
                          (rest (cddr pairs)))
                      (successive-merge (adjoin-set
                                         (make-code-tree first
                                                         second)
                                         rest))))))
      (successive-merge
       (make-leaf-set pairs)))))
