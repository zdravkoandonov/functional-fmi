#lang racket
(define (sum a b c)
  (+ a b c))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (div a b)
  (/ (- a (remainder a b)) b))

(define (sqr x)
  (* x x))

(define (qpow x y)
  (if (= y 0)
      1
      (if (= 1 (remainder y 2))
          (* x (sqr (pow x (/ (- y 1) 2))))
          (sqr (pow x (/ y 2))))))

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

(define (sum_of_interval a b)
  (* (/ (+ a b) 2) (+ 1 (- b a))))

(define (digit_cnt n)
  (if (> n 0)
      (+ 1 (digit_cnt (div n 10)))
      0))

(define (bits_cnt n)
  (if (> n 0)
      (+ 1 (bits_cnt (div n 2)))
      0))

(define (reverse n)
  (if (> n 0)
      (+ (* (remainder n 10) (pow 10 (- (digit_cnt n) 1))) (reverse (div n 10)))
      0))

(define (to_bin_rev n)
  (if (> n 0)
      (+ (* (remainder n 2) (pow 10 (- (bits_cnt n) 1))) (to_bin_rev (div n 2)))
      0))

(define (to_bin n)
  (reverse (to_bin_rev n)))
      