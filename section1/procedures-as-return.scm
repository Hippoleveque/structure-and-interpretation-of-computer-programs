#|
Section 1.3.4 and all the exercises
This file contains procedures that returns other procedures
|#


(define (average-damping f)
    (lambda(x) (average x (f x)))
)

(define (sqrt x)
    (fixed-point (average-damping (lambda(y) (/ x y)) x) 1)
)

(define (cube-root x)
    (fixed-point (average-damping (lambda(y) (/ x (square y))) x) 1.)
)

(define dx 0.00001)

(define (deriv f)
    (lambda (x) 
        (/ (- (f (+ x dx)) (f x)) 
        dx))
)

(define (cube x)
    (* x x x)
)

(define (newton-transform f)
    (lambda(x) (- x (/ (f x) ((deriv f) x))))
)

(define (newton-method f guess)
    (fixed-point (newton-transform f) guess)
)

(define (sqrt-newton x)
    (newton-method (lambda(y) (- (square y) x)) 1.)
)

(define (fixed-point-of-transform f transform guess)
    (fixed-point (transform f) guess)
)

(define (sqrt-general x)
    (fixed-point-of-transform 
        (lambda(y) (/ x y))
        average-damping 
        1.0
    )
)

(define (cubic a b c)
    (lambda (x)
        (+ (cube x) (* a (square x)) (* b x) c)
    )
)

(define (double f)
    (lambda (x) 
        (f (f x))
    )
)

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)

(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))
    )
)

(define dx 0.00001)

(define (smooth f)
    (lambda(x) 
        (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)
    )
)

(define (iterative-improve good-enough? improve)
    (define (iter guess)
        (newline)
        (display guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))
        )
    )
    (lambda(guess) (iter guess))
)

(define tolerance 0.01)

(define (good-enough-sqrt x guess) 
    ( > tolerance (abs(- x (square guess))))
)

(define (sqrt-iterative-improve x)
    ((iterative-improve (lambda(guess) (good-enough-sqrt x guess)) (average-damping (lambda(y) (/ x y)))) 1.0)
)

(define (fixed-point-iterative-improve f first-guess)
    (define (close-enough? x)
        ( > tolerance (abs (- x (f x))))
    )
    ((iterative-improve close-enough? f) first-guess)
)