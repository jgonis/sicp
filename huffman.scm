(include "Multisets.sld")
(import (scheme file))
(import (scheme write))
(import (scheme process-context))

(define-library (sicp huffman-base)
  (export make-leaf
          leaf?
          leaf-symbol
          leaf-weight
          left-tree
          right-tree
          tree-symbols
          tree-weight)
  (import (scheme base)
          (scheme cxr))
  (begin
    (define (make-leaf symbol weight)
      (list 'leaf symbol weight))
    (define (leaf? node)
      (eq? (car node) 'leaf))
    (define (leaf-symbol leaf)
      (cadr leaf))
    (define (leaf-weight leaf)
      (caddr leaf))
    (define (left-tree tree)
      (car tree))
    (define (right-tree tree)
      (cadr tree))
    (define (tree-symbols tree)
      (cond ((leaf? tree) (list (leaf-symbol tree)))
            (else (caddr tree))))
    (define (tree-weight tree)
      (cond ((leaf? tree) (leaf-weight tree))
            (else (cadddr tree))))))

(define-library (sicp huffman-encoding)
  (export make-code-tree
          adjoin-set
          make-leaf-set
          encode)
  (import (scheme base)
          (sicp huffman-base))
  (begin
    (define (make-code-tree left-tree right-tree)
      (list left-tree
            right-tree
            (append (tree-symbols left-tree)
                    (tree-symbols right-tree))
            (+ (tree-weight left-tree)
               (tree-weight right-tree))))
    (define (adjoin-set x set)
      (cond ((null? set) (list x))
            ((< (tree-weight x) (tree-weight (car set)))
             (cons x set))
            (else (cons (car set) (adjoin-set x (cdr set))))))
    
    (define (make-leaf-set pairs)
      (cond ((null? pairs) '())
            (else (let ((pair (car pairs)))
                    (adjoin-set (make-leaf (car pair)
                                                   (cadr pair))
                                        (make-leaf-set (cdr pairs)))))))
    (define (encode message tree)
      (cond ((null? message) '())
            (else (append (encode-symbol (car message)
                                         tree)
                          (encode (cdr message)
                                  tree)))))
    (define (encode-symbol symbol tree)
      (define (go-left? symbol symbol-list)
        (cond ((null? symbol-list) #f)
              ((eq? symbol (car symbol-list)) #t)
              (else (go-left? symbol (cdr symbol-list)))))
      (cond ((leaf? tree) (cond ((eq? (leaf-symbol tree) symbol) '())
                                (else (error "unknown symbol!"))))
            ((go-left? symbol (tree-symbols (left-tree tree)))
             (cons 0 (encode-symbol symbol (left-tree tree))))
            (else (cons 1 (encode-symbol symbol (right-tree tree))))))))

(define-library (sicp huffman-decoding)
  (export decode)
  (import (scheme base)
          (sicp huffman-base))
  (begin
    (define (decode bits tree)
      (define (choose-branch bit tree)
        (cond ((= bit 0) (left-tree tree))
              ((= bit 1) (right-tree tree))
              (else (error "bad bit!"))))
      (define (decode-1 bits current-branch)
        (cond ((null? bits) '())
              (else (let ((next-branch (choose-branch (car bits)
                                                      current-branch)))
                      (cond ((leaf? next-branch)
                             (cons (leaf-symbol next-branch)
                                   (decode-1 (cdr bits) tree)))
                            (else (decode-1 (cdr bits)
                                            next-branch)))))))
      (decode-1 bits tree))))


(define-library (sicp generate-huffman)
  (export generate-huffman-tree)
  (import (scheme base)
          (sicp huffman-encoding)
          (sicp huffman-base))
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
