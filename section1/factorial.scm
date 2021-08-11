#|
Section 1.2.1
This file contains the procedures to compute the factorial of an integer
|#


(define (factorial n)
; Factorial computation as a recursive procedure: memory-wise inefficient
    (if (= n 1) 1 (
        * n (factorial (- n 1))
    ))
)


(define (fact n)
    (fact-iter 1 1 n)
)

(define (fact-iter counter currProd n)
; Factorial computation as an iterative process: O(1) complexity and memory
    (if (> counter n) currProd (
        fact-iter (+ counter 1) (* counter currProd) n
    ))
)