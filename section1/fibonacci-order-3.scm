#|
Section 1.2.2 - Exercise 1.11
This file contains procedures to compute the finoacci sequence order 3
|#


(define (f n)
; Naive implementation of recursive process
    (if (< n 3) n 
        (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))
    )
)

(define (smartf n)
    (f-iter 0 1 2 (- n 2))
)

(define (f-iter a b c count)
; Efficient implementation as an iterative process
    (if (= count 0) c
        (f-iter b c (+ a b c) (- count 1))
    )
)