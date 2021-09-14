#|
Section 1.3.3
This file contains exemples of general methods
And all exercises of section 1.3.3
|#

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value) (search neg-point midpoint))
                ((negative? test-value) (search midpoint pos-point))
                (else midpoint)
                )
            )
        )
    )    
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        ( > tolerance (abs (- v1 v2)))
    )
    (define (try-guess guess)
        (let ((next (f guess)))
            (newline)
            (display next)
            (if (close-enough? guess next)
                next
                (try-guess next)
            )
        )
    )
    (try-guess first-guess)
)

(define (cont-frac n d k)
    (define (cont-frac-rec n d k j)
        (if (= 1 k)
        (/ (n j) (d j))
        (/ (n j) (+ (d j) (cont-frac-rec n d (- k 1) (+ 1 j))))
        )
    )
    (cont-frac-rec n d k 1)
)

(define (cont-frac-smart n d k)
    (define (cont-frac-iter res n d k)
        (if (= 0 k)
            res
            (cont-frac-iter (/ (n k) (+ res (d k))) n d (- k 1))
        )
    )
    (cont-frac-iter 0 n d k)
)

(define (deuler k)
    (if (= 0 (remainder (+ k 1) 3)) 
        (* 2 (/ (+ k 1) 3))
        1
    )
)

(define (tan-cf x k)
    (define (nlambert n)
        (if (= n 1)
            x
           (- (square x))
        )
    )
    (define (dlambert n)
        (- (* 2 n) 1)
    )
    (cont-frac-smart nlambert dlambert k)
)