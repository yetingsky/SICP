#lang sicp
(#%require sicp-pict)

; factorial
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
                   (+ counter 1))))
  (iter 1 1))

; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; Fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

; Count Change
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; ex 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (> count n)
        a
        (iter (+ a (* 2 b) (* 3 c)) a b)))
  (if (< n 3)
      n
      (iter 2 1 0)))

; ex 1.12
(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))

; Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))

; ex 1.16
(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
          ((even? n) (iter (square b) (/ n 2) a))
          (else (iter b (- n 1) (* a b)))))
  (iter b n 1))

; ex 1.17
(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))

(define (double x) (* x 2))
(define (halve x) (/ x 2))

; ex 1.18
(define (fast-mul-iter a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

; ex 1.19
(define (fast-fib n)
  (define (fast-fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else
           (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))
  (fast-fib-iter 1 0 0 1 n))

; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; primality
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))