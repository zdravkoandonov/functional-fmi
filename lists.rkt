#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (flatten x)
  (if (empty? x)
      '()
      (append (if (atom? (car x))
                  (list (car x))
                  (flatten (car x)))
              (flatten (cdr x)))))

(flatten '(1 (2 3 4) (5 (6)) (7 8 (9)) (((10 11) (12 (13) 14) 15 16) 17)))
(flatten '((((())))))
(flatten '(() ()))

(define (filter!= arr x)
  (if (empty? arr)
      '()
      (if (= (car arr) x)
          (filter!= (cdr arr) x)
          (append (list (car arr)) (filter!= (cdr arr) x)))))

(filter!= '(1 2 3 4 5 1 1 2 3 2 4 5) 2)

(define (reverse arr)
  (if (empty? arr)
      '()
      (append (reverse (cdr arr)) (list (car arr)))))

(reverse '(1 2 3 4 5))

; returns first n elements in L - if n > size(L) return L
(define (first L n)
  (if (and (> n 0) (not (empty? L)))
      (cons (car L) (first (cdr L) (- n 1)))
      '()))

(first '(1 2 3 4 5 6) 4)
(first '(1 2 3 4 5 6) 8)
(first '() 2)

(define (map-cons L n)
  (if (= (length (first L n)) n)
      (cons (first L n) (map-cons (cdr L) n))
      '()))

(map-cons '(1 2 3 4 5 6) 2)
(map-cons '(1 2 3 4 5 6) 4)

;(define (map L proc)
;  (if (empty? L)
;      '()
;      (cons (proc (car L)) (map (cdr L) proc))))
;
;(map '(1 2 3 4 5 6) (lambda (x) (* x x)))

(define (filter L pred)
  (if (empty? L)
      '()
      (if (pred (car L))
          (cons (car L) (filter (cdr L) pred))
          (filter (cdr L) pred))))

(filter '(1 2 3 4 5 6) odd?)
(filter '(1 2 3 4 5 6) even?)

(define (reduce L op)
  (if (empty? (cdr L))
      (car L)
      (op (car L) (reduce (cdr L) op))))

(reduce '(1 2 3 4 5) +)
(reduce '(1 2 3 4 5) *)

; consecutive
(define L '(1 2 3 4 5))
(map (lambda (a b) (cons a (list b))) L (append (cdr L) (list (car L))))

(define (map-with-index L proc)
  (define (map-with-index-iter L index)
    (if (empty? L)
        '()
        (cons (proc (car L) index) (map-with-index-iter (cdr L) (+ index 1)))))
  (map-with-index-iter L 0))

(map-with-index '(1 2 3 4) (lambda (element index) (expt element index)))