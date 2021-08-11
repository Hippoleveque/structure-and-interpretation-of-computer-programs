#|
Section 1.2.1 - Exercise 1.10
This file contains the procedure to compute the Ackermann number of two real numbers
|#


(define (A x y)
    (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))
    )
)

