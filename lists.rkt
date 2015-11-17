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