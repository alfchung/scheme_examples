#lang scheme

; Rational number "class"

(define (make-rat n d) (cons n d))
(define (get-num x) (car x))
(define (get-dem x) (cdr x))
(define (get-real-value x) (/ (* 1.0 (get-num x)) (get-dem x)))
(define (get-rat-value x) (/ (get-num x) (get-dem x)))



(get-real-value (make-rat 1 3))
(get-real-value (make-rat 1 3))
(get-num (make-rat 1 3))
(get-dem (make-rat 1 3))

