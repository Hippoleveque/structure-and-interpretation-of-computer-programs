#| 
Section 1.1.7 - Exercise 1.8 
This file contains the procedures to approximate the square root of a number
|#


(define (cube-root x)
    (iter-cbr 1.0 0.5 x)
)


(define (iter-cbr guess lastGuess x)
; iterative process to find the square root of a real number
    (if (good-iter? guess lastGuess) guess
    (iter-cbr (improve guess x) guess x))
)


(define (good-iter? guess lastGuess)
; Test whereas our solution is good enough
    (< (abs (/ (- guess lastGuess) guess)) 0.0001)
)

(define (square x)
    (* x x)
)

(define (abs x)
    (if (< 0 x) x (- x))
)

(define (improve guess x)
; The procedure to improve our approximations of the cube root of real number x
    (/ (+ (/ x (square guess))(* 2 guess)) 3)
)

