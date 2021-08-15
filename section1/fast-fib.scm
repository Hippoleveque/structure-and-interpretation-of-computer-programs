#|
Section 1.2.4 & Exercise 1.19
This file contains the set of procedures that can compute the 
Fibonacci numbers in an efficient way
|#

(define (fib n)
    (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
    (cond ((= 0 count) b)
    ((even? count) 
        (fib-iter a b (+ (square q) (square p)) (+ (square q) (* 2 p q)) (/ count 2))
    )
    (else (fib-iter (+ (* a q) (* b q) (* a p)) (+ (* b p ) (* a q)) p q (- count 1)))
    )
)

