#|
Section 1.2.4 & Exercise 1.17 - 1.18
This file contains procedures to compute the multiplication of a real number
|#


(define (slow-mul a b)
    (if (= 0 b) 0
        (+ a (* a (- b 1)))
    )
)

(define (double x)
    (+ x x)
)

(define (halve x)
    (/ x 2)
)

(define (fast-mul a b)
    (fast-mul-iter 0 a b)
)


(define (bad-fast-mul a b)
    (cond ((= b 0) 0)
    ((= 0 (remainder b 2)) (bad-fast-mul (double a) (halve b)))
    (else (+ a (bad-fast-mul a (- b 1))))
))

(define (fast-mul-iter c a b)
; Clever multiplication as an iterative process O(1) in memory and O(log(b)) in complexity
; Keeping a + c the same 
    (cond ((= b 0) c)
    ((= 0 (remainder b 2)) (fast-mul-iter c (double a) (halve b)))
    (else (fast-mul-iter (+ a c) a (- b 1)))
    )
)


#|
Maybe I can reduce complexity to O(log(min(a,b)))
|#