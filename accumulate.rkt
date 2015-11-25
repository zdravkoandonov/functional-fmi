#lang racket
(define (accumulate op term null-value a next b)
  (if (> a b) null-value
      (op (term a)
          (accumulate op term null-value (next a) next b))))

(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

; x + x^2 + x^4 + ... + x^(2^n)
(define (sum_of_powers_of_two x n)
  (accumulate + (lambda (p) (pow x (pow 2 p))) 0 0 (lambda (x) (+ x 1)) n))

; count of stationary points of f in [a, b] (x is stationary point if f(x) = x)
(define (count_of_stationary_points f a b)
  (accumulate + (lambda (x) (if (= (f x) x) 1 0)) 0 a (lambda (x) (+ x 1)) b))

; sum of stationary points of f in [a, b] (x is stationary point if f(x) = x)
(define (sum_of_stationary_points f a b)
  (accumulate + (lambda (x) (if (= (f x) x) x 0)) 0 a (lambda (x) (+ x 1)) b))

; f(0) + f(1) + ... + f(n)
(define (sum_of_functions f n)
  (accumulate + f 0 0 (lambda (x) (+ x 1)) n))

(define (is_prime n)
  (define (div_iter n d)
    (if (<= d (expt n 0.5))
        (and (not (= (remainder n d) 0)) (div_iter n (+ d 1)))
        #t))
  (if (= n 1)
      #f
      (div_iter n 2)))

; sum of all primes in [a, b] ending in 3
; TODO
