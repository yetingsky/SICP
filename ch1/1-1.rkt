#lang sicp
(#%require sicp-pict)

; ex 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; ex 1.3
(define (max-square-two-of-three x y z)
  (cond ((and (<= x y) (<= x z)) (+ (* y y) (* z z)))
        ((and (<= y x) (<= y z)) (+ (* x x) (* z z)))
        ((and (<= z x) (<= z y)) (+ (* x x) (* y y)))))

; ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; ex 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; Newton Method
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

; ex 1.8
(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  (define (cube-iter guess)
    (if (good-enough? guess)
        guess
        (cube-iter (improve guess))))
  (cube-iter 1.0))

(define (cube x) (* x x x))