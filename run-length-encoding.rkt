#lang racket
(define (group L)
  (define (helper current-group xs)
    (cond ((null? xs) (list current-group))
          ((= (car xs) (car current-group)) (helper (cons (car xs) current-group) (cdr xs)))
          (else (append (list current-group) (helper (list (car xs)) (cdr xs))))))
  (if (null? L)
      L
      (helper (list (car L)) (cdr L))))

(group (list 0 0 0 1 1 2 3 3 3 3 4 4))
(group (list 1 1 1 2 3 1 2 2))
(group (list 1 2 3 4 5 6))
(group (list 1 1 1 1 1 1))
(group (list))