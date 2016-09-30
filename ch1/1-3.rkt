#lang sicp
(#%require sicp-pict)

; higher-order function
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-integers a b)
  (define (identity x) x)
  (sum identity a inc b))

(define (sum-cubes a b)
  (define (cube x) (* x x x))
  (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; ex 1.29
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (odd-term x) (* 4 (f x)))
  (define (even-term x) (* 2 (f x)))
  (define (next x) (+ x (* 2 h)))
  (* (/ h 3)
  (+ (sum odd-term (+ a h) next (+ a (* (- n 1) h)))
     (sum even-term (+ a h h) next (+ a (* (- n 2) h)))
     (f a)
     (f (+ a (* n h))))))

; ex 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; ex 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-product a b)
  (define (term x) (/ (* (- x 1) (+ x 1)) (* x x)))
  (define (next x) (+ x 2))
  (* 4
     (product term a next b)))

; ex 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; ex 1.33
(define (accumulate-filter combiner filter null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a) (accumulate-filter combiner filter null-value term (next a) next b)))
        (else
         (accumulate-filter combiner filter null-value term (next a) next b))))

; using lambada and let
(define (square x) (* x x))

(define (f x y)
  ((lambda (a b)
     (+ (* x (* square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; a let expression is simply syntactic sugar for the underlying lambda application
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; find roots of equations by the half-interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (average a b) (/ (+ a b) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

; finding fixed points of functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; ex 1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; ex 1.36
(define (fixed-point-modify f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define log1000-1
  (fixed-point-modify (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2))
(define log1000-2
  (fixed-point-modify (lambda (x) (/ (log 1000) (log x))) 2))

; ex 1.37
(define (cont-frac n d k)
  (define (iter i)
    (let ((di (d i))
          (ni (n i)))
      (if (= k i)
          (/ ni di)
          (/ ni (+ di (iter (+ i 1)))))))
  (iter 1))
 
; ex 1.38
(define (e k)
  (define (N i) 1)
  (define (D i)
    (if (= 0 (remainder (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1))
  (+ 2.0 (cont-frac N D k)))

; ex 1.39
(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (D i) (- (* 2 i) 1))
  (cont-frac N D k))

; procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; newton's method
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-3 x)
  (newton-method
   (lambda (y) (- (square y) x)) 1.0))

; ex 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

; ex 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

; ex 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; ex 1.43
(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((repeated f (- n 1)) (f x)))))

; ex 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

; ex 1.46
(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (sqrt-4 x)
  (define dx 0.00001)
  (define (good-enough? x y)
    (< (abs (- x y)) dx))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (iterative-improve good-enough? improve) 1.0)
    
