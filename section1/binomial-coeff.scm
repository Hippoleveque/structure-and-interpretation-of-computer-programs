#|
Section 1.2.2 - Exercise 1.12
This file contains procedures to compute the binomial coefficients
|#


(define (bin k n)
    (cond ((or (< k 0) (< n k)) 0)
          ((= k 0) 1)
          (else (+ (bin (- k 1) (- n 1)) (bin k (- n 1))))
    )
)

