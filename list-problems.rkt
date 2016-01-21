#lang racket

; Намира сумата на всички числа в numbers
; -> (sum (list))
; 0
; -> (sum (list 1 2 3))
; 6
(define (sum numbers)
  (if (null? numbers)
      0
      (+ (car numbers) (sum (cdr numbers)))))

(sum (list))
(sum (list 1 2 3))


; Проверява дали x се среща в items
; -> (member? 1 (list 1 2 3))
; #t
; -> (member? "asdf" (list "asd"))
; #f
; Разгледайте http://docs.racket-lang.org/reference/booleans.html
(define (member? x items)
  (if (null? items)
      #f
      (or (eq? x (car items)) (member? x (cdr items)))))

(member? 1 (list 1 2 3))
(member? "asdf" (list "asd"))

; -> (length2 (range2 1 10))
; 9
; В Racket има такава функция, наречена length
(define (length2 items)
  (if (null? items)
      0
      (+ 1 (length2 (cdr items)))))

(length2 (range 1 10))

; Връща n-тия елемент от items при 0лево базиран индекс
; -> (list-ref2 (list 1 2 3) 0)
; 1
; В Racket има такава функция, наречена list-ref
(define (list-ref2 items n)
  (define (helper i xs)
    (if (= i n)
        (car xs)
        (helper (+ i 1) (cdr xs))))
  (helper 0 items))

(list-ref2 (list 1 2 3) 0)

; -> (range2 1 10)
; '(1 2 3 4 5 6 7 8 9)
; В Racket съществува такава функция, наречена range
(define (range2 a b)
  (if (= a b)
      (list)
      (cons a (range2 (+ a 1) b))))

(range2 1 10)

; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
; i-тия елемент на този списък е (f i)
; -> (build-list2 3 id)
; '(0 1 2)
; -> (build-list2 3 (lambda (x) (* x x)))
; '(0 1 4)
; В Racket има такава функция, наречена build-list
(define (build-list2 n f)
  (define (helper i)
    (if (= i n)
        (list)
        (cons (f i) (helper (+ i 1)))))
  (helper 0))

(define (id x) x)
(build-list2 3 id)
(build-list2 3 (lambda (x) (* x x)))

; Конкатенира два списъка в нов списък
; -> (append2 (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)
; В Racket има такава фунцкия, наречена append
(define (append2 l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append2 (cdr l1) l2))))

(append2 (list 1 2 3) (list 4 5 6))

; Обръща списъка наобратно
; -> (reverse2 (list 1 2 3))
; '(3 2 1)
; В Racket има такава функция, наречена reverse
(define (reverse2 items)
  (if (null? items)
      (list)
      (append (reverse2 (cdr items)) (list (car items)))))

(reverse2 (list 1 2 3))

; Взима първите n елемента от даден списък
; Ако (> n (length items)), тогава връща items
; -> (take2 3 (list 1 2 3 4 5))
; '(1 2 3)
(define (take2 n items)
  (define (helper i xs)
    (if (or (= i n) (null? xs))
        (list)
        (cons (car xs) (helper (+ i 1) (cdr xs)))))
  (helper 0 items))

(take2 3 (list 1 2 3 4 5))

; Маха първите n елемента от даден списък
; Ако (> n (length items)) връща '()
; -> (drop2 3 (list 1 2 3 4 5))
; '(4 5)
(define (drop2 n items)
  (define (helper i xs)
    (if (or (= i n) (null? xs))
        xs
        (helper (+ i 1) (cdr xs))))
  (helper 0 items))

(drop2 3 (list 1 2 3 4 5))
        

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
; -> (take-while zero? (list 0 0 0 1 2 3))
; '(0 0 0)
; -> (take-while even? (list 2 4 5 7 8 3 2))
; '(2 4)
; -> (take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '()
(define (take-while p items)
  (cond ((null? items) (list))
        ((p (car items)) (cons (car items) (take-while p (cdr items))))
        (else (list))))
      
(take-while zero? (list 0 0 0 1 2 3))
(take-while even? (list 2 4 5 7 8 3 2))
(take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))

; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава лъжа за тях
; -> (drop-while zero? (list 0 0 0 1 2 3))
; '(1 2 3)
; -> (drop-while even? (list 2 4 5 7 8 3 2))
; '(5 7 8 3 2)
; -> (drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '(1 1 1 1 1)
(define (drop-while p items)
  (cond ((null? items) (list))
        ((p (car items)) (drop-while p (cdr items)))
        (else items)))

(drop-while zero? (list 0 0 0 1 2 3))
(drop-while even? (list 2 4 5 7 8 3 2))
(drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))

; Функцията взима число и връща списък от цифрите му
; -> (number->list 123)
; '(1 2 3)
(define (number->list n)
  (define (reversed m)
    (if (= m 0)
      (list)
      (cons (remainder m 10) (reversed (quotient m 10)))))
  (if (= n 0)
      (list 0)
      (reverse (reversed n))))

(number->list 123)

; Функцията взима списък от цифри и връща числото
; -> (list->number (list 1 2 3))
; 123
(define (list->number ns)
  (define (at-index L n)
    (if (= n 0)
        (car L)
        (at-index (cdr L) (- n 1))))
  (define (helper i xs)
    (if (null? xs)
        0
        (+ (* (expt 10 i) (car xs)) (helper (- i 1) (cdr xs)))))
  (helper (- (length ns) 1) ns))

(list->number (list 0))
(list->number (list 1))
(list->number (list 1 2 3))