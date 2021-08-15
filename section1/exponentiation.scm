#|
Section 1.2.4 & Exercise 1.16
This file contains procedures to compute the exponentiation of a real number
|#


(define (slow-exp b n)
; Naive implementation O(n) complexity and memory
    (if (= n 0) 1
        (* b (slow-exp b (- n 1)))
    )
)

(define (fast-exp b n)
; Smarter implementation O(lg(n)) complexity and memory 
    (cond ((= n 0) 1)
    ((= (remainder n 2) 0) (square (fast-exp b (/ n 2)))) 
    ( else (* b (square (fast-exp b (/ (- n 1) 2))))))
)


(define (square b)
    (* b b)
)

(define (exp b n)
    (iter-exp 1 b n)
)

(define (iter-exp a b n)
; Clever implementation of iterative process O(1) memory O(lg(n)) complexity
; Keep a * b^n the same 
    (cond ((= n 0) a)
    ((= (remainder n 2) 0) (iter-exp a (square b) (/ n 2)))
    (else (iter-exp (* a b) b (- n 1)))
    )
)