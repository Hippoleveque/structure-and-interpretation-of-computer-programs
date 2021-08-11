#| 
Section 1.1.7
This file contains the procedures given as examples to compute 
the square root of a real number
|#


(define (sumSquare x y)
    (+ (* x x) (* y y))
)

(define (maxSquare x y z)
  (if (< x y) (if (< x z) (sumSquare z y) (sumSquare x y)) (if (< y z) (sumSquare x z) (sumSquare x y)))
)


(define (sqrt x) 
  (iter-sqrt 1.0 0.5 x)
)

(define (find-sqrt guess x)
; recursive process to approximate the square root of a real number
    (if (good-enough? guess x) guess
    (find-sqrt (average guess (/ x guess)) x))
)

(define (iter-sqrt guess lastGuess x)
; iterative process to approximate the square root of a real number
    (if (good-iter? guess lastGuess) guess
    (iter-sqrt (average guess (/ x guess)) guess x))
)

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001)
)

(define (good-iter? guess lastGuess)
    (< (abs (/ (- guess lastGuess) guess)) 0.0001)
)

(define (square x)
    (* x x)
)

(define (abs x)
    (if (< 0 x) x (- x))
)

(define (average x y)
    (/ (+ x y) 2)
)