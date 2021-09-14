#|
Section 2.1.3
|#

(define (cons x y)
    (define (dispatch m)
        (cond ((= 0 m) x)
        ((= 1 m) y)
        (else (error "Argument not 0 or 1 -- CONS")))
    )
    dispatch
)

(define (car z) (z 0))

(define (cdr z) (z 1))


(define (cons-bis x y)
    (lambda (m) (m x y))
)

(define (car-bis z)
    (z (lambda (p q) p))
)

(define (cdr-bis z)
    (z (lambda (p q) q))
)


(define (cons-math x y)
    (lambda (m) (m (* (power 2 x) (power 3 y))))
)

(define (car-math z)
    (define (extract-power n)
        (define (extract-power-iter n power)
            (if (= 1 (remainder n 2))
                power
                (extract-power-iter (/ n 2) (+ 1 power))
            )
        )
        (extract-power-iter n 0)
    )
    (z extract-power)
)

(define (cdr-math z)
    (define (extract-power n)
        (define (extract-power-iter n power)
            (if (= 0 (remainder n 3))
                (extract-power-iter (/ n 3) (+ 1 power))
                power    
            )
        )
        (extract-power-iter n 0)
    )
    (z extract-power)
)

(define zero (lambda (f) (lambda(x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x))))
)

(define (add n p) 
    (lambda (f) (lambda (x) ((n f) ((p f) x))))
)



