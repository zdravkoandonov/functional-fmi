#lang racket
(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (list node '() '()))

(define t
  (make-tree 1
    (make-tree 2
      (make-leaf 5)
      (make-leaf 6))
    (make-leaf 3)))

(define (root tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (count-nodes tree)
  (if (null? tree)
      0
      (+ 1 (count-nodes (left tree)) (count-nodes (right tree)))))

(count-nodes t)

(define (height tree)
  (if (null? tree)
      0
      (+ 1 (max (height (left tree)) (height (right tree))))))

(height t)

(define (tree-level level tree)
  (define (helper current-level t)
    (cond ((null? t) (list))
          ((= current-level level) (list (root t)))
          (else (append (helper (+ current-level 1) (left t)) (helper (+ current-level 1) (right t))))))
  (helper 1 tree))    
    
(tree-level 1 t)
(tree-level 2 t)
(tree-level 3 t)

(define (tree-levels tree)
  (let ((tree-height (height tree)))
    (define (helper level)
      (if (> level tree-height)
          (list)
          (cons (tree-level level tree) (helper (+ level 1)))))
    (helper 1)))

(tree-levels t)

(define (tree-map f tree)
  (if (null? tree)
      (list)
      (make-tree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))))

(tree-map add1 t)

(define bst
  (make-tree 10
             (make-tree 5
                        (make-leaf 2)
                        (make-leaf 8))
             (make-tree 15
                        (make-leaf 12)
                        (make-leaf 17))))

; Inserts x in the tree, returning a new BST with the proper structure
(define (bst-insert x tree)
  (cond ((null? tree) x)
        ((< x (root tree)) (make-tree (root tree) (bst-insert x (left tree)) (right tree)))
        (else (make-tree (root tree) (left tree) (bst-insert x (right tree))))))

(bst-insert 1 bst)
(bst-insert 3 bst)
(bst-insert 7 bst)
(bst-insert 9 bst)

; Checks if x is an element of tree
(define (bst-element? x tree)
  (cond ((null? tree) #f)
        ((= x (root tree)) #t)
        ((< x (root tree)) (bst-element? x (left tree)))
        (else (bst-element? x (right tree)))))

(bst-element? 8 bst)
(bst-element? 12 bst)
(bst-element? 13 bst)

; Traverse the tree in a such way that the list should contain sorted elements
(define (bst->list tree)
  (if (null? tree)
      (list)
      (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))))

(bst->list bst)

; Checks if the given binary tree is a binary search tree
(define (bst? tree)
  (and (or (null? (left tree)) (and (< (root (left tree)) (root tree)) (bst? (left tree))))
       (or (null? (right tree)) (and (>= (root (right tree)) (root tree)) (bst? (right tree))))))

(bst? bst)
(bst? t)