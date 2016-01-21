#lang racket
(define (mon-change L)
  (define (helper i xs)
    (cond ((< (length xs) 3) (list))
          ((< (car xs) (cadr xs)) (if (< (cadr xs) (caddr xs)) (helper (+ i 1) (cdr xs)) (cons (+ i 2) (helper (+ i 1) (cdr xs)))))
          (else (if (< (cadr xs) (caddr xs)) (cons (+ i 2) (helper (+ i 1) (cdr xs))) (helper (+ i 1) (cdr xs))))))
  (helper 1 L))

(mon-change (list 1 3 5 6 4 2 1 9 12 16 14 13 10 16 18 20 22))
(mon-change (list 10 8 6 4 1 3 5 6 4 2 1 9 12 16 14 13 10))

(define (mon-min-max L)
  (define (helper xs)
    (cond ((null? xs) (list))
          ((null? (cdr xs)) (list (car xs)))
          ((null? (cddr xs)) (list (cadr xs)))
          ((< (car xs) (cadr xs)) (if (< (cadr xs) (caddr xs)) (helper (cdr xs)) (cons (cadr xs) (helper (cdr xs)))))
          (else (if (< (cadr xs) (caddr xs)) (cons (cadr xs) (helper (cdr xs))) (helper (cdr xs))))))
  (helper L))

(mon-min-max (list 10 8 6 4 1 3 5 6 4 2 1 9 12 16 14 13 10))
(mon-min-max (list 1 3 5 6 4 2 1 9 12 16 14 13 10 16 18 20 22))

(define (apply-on-matrix Lf)
  (define (apply-on-array Lf L)
    (if (null? L)
        (list)
        (cons ((car Lf) (car L)) (apply-on-array (cdr Lf) (cdr L)))))
  (define (apply-on-rows M)
    (if (null? M)
        (list)
        (cons (apply-on-array Lf (car M)) (apply-on-rows (cdr M)))))
  (lambda (M) (apply append (apply-on-rows M))))

(define (1+ x) (+ 1 x))
(define (2+ x) (+ 2 x))
(define (3+ x) (+ 3 x))
(define (4+ x) (+ 4 x))

(define M (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12) (list 13 14 15 16)))

((apply-on-matrix (list 1+ 2+ 3+ 4+)) M)

(define (infinite1 f L)
  (define (helper xs)
    (if (null? xs)
        (helper L)
        (stream-cons (f (car xs)) (helper (cdr xs)))))
  (helper L))

(define (infinite2 f L)
  (let ((rev (reverse L)))
    (define (helper xs)
       (if (null? xs)
           (helper rev)
           (stream-cons (f (car xs)) (helper (cdr xs)))))
    (helper rev)))

(define (infinite3 n)
  (define (helper i ascending)
    (cond ((= i n) (stream-cons i (helper (- i 1) (not ascending))))
          ((= i 1) (stream-cons i (helper (+ i 1) (not ascending))))
          (ascending (stream-cons i (helper (+ i 1) ascending)))
          (else (stream-cons i (helper (- i 1) ascending)))))
  (helper n #t))

(define (infinite4 n)
  (define (helper i ascending)
    (cond ((= i n) (stream-cons i (helper (- i 1) (not ascending))))
          ((= i 1) (stream-cons i (helper (+ i 1) (not ascending))))
          (ascending (stream-cons i (helper (+ i 1) ascending)))
          (else (stream-cons i (helper (- i 1) ascending)))))
  (helper 1 #f))

(define (stream-take n stream)
  (define (helper i s)
    (if (= i n)
        (list)
        (cons (stream-first s) (helper (+ i 1) (stream-rest s)))))
  (helper 0 stream))

(stream-take 20 (infinite1 1+ (list 1 2 3 4)))
(stream-take 20 (infinite2 1+ (list 1 2 3 4)))
(stream-take 20 (infinite3 4))
(stream-take 20 (infinite4 4))

(define (has-path tree path)
  (define (helper t ps)
    (cond ((null? ps) #t)
          ((null? t) #f)
          ((equal? (car t) (car ps)) (or (helper (cadr t) (cdr ps)) (helper (caddr t) (cdr ps))))
          (else (or (helper (cadr t) path) (helper (caddr t) path)))))
  (helper tree path))

(define (make-tree root left right)
  (list root left right))

(define (empty-tree)
  (list))

(define t (make-tree 1 
            (make-tree 3
                       (make-tree 5
                                  (empty-tree)
                                  (empty-tree))
                       (make-tree 6
                                  (make-tree 8 (empty-tree) (empty-tree))
                                  (empty-tree)))
            (make-tree 9
                       (empty-tree)
                       (make-tree 7
                                  (make-tree 2 (empty-tree) (empty-tree))
                                  (make-tree 1 (empty-tree) (empty-tree))))))

(has-path t (list 1 9 7 2))
(has-path t (list 1 9 7))
(has-path t (list 9 7))
(has-path t (list 7))
(has-path t (list 9 1))
(has-path t (list 1 3 5 6))

(define t-sym (make-tree 'a
            (make-tree 'b
                       (make-tree 'c
                                  (empty-tree)
                                  (empty-tree))
                       (make-tree 'd
                                  (make-tree 'e (empty-tree) (empty-tree))
                                  (empty-tree)))
            (make-tree 'f
                       (empty-tree)
                       (make-tree 'g
                                  (make-tree 'h (empty-tree) (empty-tree))
                                  (make-tree 'i (empty-tree) (empty-tree))))))

(has-path t-sym (list 'a 'b 'd 'e))
(has-path t-sym (list 'a 'b 'd))
(has-path t-sym (list 'b 'd))
(has-path t-sym (list 'd))
(has-path t-sym (list 'a 'd))
(has-path t-sym (list 'a 'b 'c 'd))