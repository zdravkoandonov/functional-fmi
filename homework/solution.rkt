#lang racket
(define (f p g h)
  (lambda (x) (and (p (g x)) (p (h x)))))

(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Simplify, if possible, the given fraction
(define (simplify-frac frac)
  (cons (/ (fst frac) (gcd (fst frac) (snd frac))) (/ (snd frac) (gcd (fst frac) (snd frac)))))

; add two fractions together and return a new fraction
(define (add-frac frac1 frac2)
  (simplify-frac (cons (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2)))))

; subtract two fractions and return a new fraction
(define (subtract-frac frac1 frac2)
  (simplify-frac (cons (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2)))))

; multiply and return a new fraction
(define (mult-frac frac1 frac2)
  (simplify-frac (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2)))))