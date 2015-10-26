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

(define (is_pal n)
  (= n (reverse n)))

(define (contains_pal_at_front n)
  (and (> (digit_cnt n) 1) (or (is_pal n) (contains_pal_at_front (div n 10)))))

(define (contains_pal n)
  (and (> (digit_cnt n) 1) (or (contains_pal_at_front n) (contains_pal (reverse (div (reverse n) 10))))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (fibn n)
  (define (fib_iter a b i)
    (if (< i n)
        (fib_iter b (+ a b) (+ i 1))
        a))
  (fib_iter 0 1 0))

(define (pal_cnt_in_interval a b)
  (define (pal_cnt_in_interval_iter i cnt)
    (if (<= i b)
        (pal_cnt_in_interval_iter (+ i 1) (+ cnt (if (is_pal i) 1 0)))
        cnt))
  (pal_cnt_in_interval_iter a 0))