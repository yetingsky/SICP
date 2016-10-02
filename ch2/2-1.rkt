#lang sicp
(#%require sicp-pict)

(define make-rat cons)
(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; ex 2.2
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (midpoint-segment l)
  (make-point (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
              (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))

; implement cons car cdr using dispatch
(define (my-cons x y)
  (define (dispatch m)
    (cons ((= m 0) x)
          ((= m 1) y)))
    dispatch)
(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

; ex 2.4
(define (my-cons-2 x y)
  (lambda (m)
    (m x y)))
(define (my-car-2 z)
  (z (lambda (p q) p)))
(define (my-cdr-2 z)
  (z (lambda (p q) q)))

; ex 2.5
(define (my-cons-3 a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (my-car-3 z)
  (if (= 0 (remainder z 2))
      (+ 1 (my-car-3 (/ z 2)))
      0))
(define (my-cdr-3 z)
  (if (= 0 (remainder z 3))
      (+ 1 (my-cdr-3 (/ z 3)))
      0))

; ex 2.6
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define +
    (lambda (m)
        (lambda (n)
            (lambda (f)
                (lambda (x)
                    (m f (n f x)))))))
