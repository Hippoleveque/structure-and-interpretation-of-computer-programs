#|
Section 1.3.1
This file contains exemples of higher-order procedures
And all exercises of section 1.3.1
|#

(define (sum term a next b)
    (define (iter res term a next b)
        (if (> a b)
            res
            (iter (+ res (term a)) term (next a) next b)
        )
    )
    (iter 0 term a next b)
)

(define (integral f a b dx)
    (define (add-dx y)
        (+ y dx)
    )
    (* (sum f (+ a (/ dx 2)) add-dx b) dx)
)

(define (simpson-integral f a b n)
    (define (odd-term x)
        (* 4 (f x))
    )
    (define (even-term x)
        (* 2 (f x))
    )
    (define (add-two-hs x)
        (+ x (* 2 (/ (- b a) n)))
    )
    
    (* (+ (sum odd-term (add-two-hs a) add-two-hs b) (sum even-term (+ a (/ (- b a ) n)) add-two-hs b) (f a) (f b)) (/ (/ (- b a) n) 3.))
)

(define (product f a next b)
    (define (iter res f a next b)
        (if (< b a)
            res
            (iter (* res (f a)) f (next a) next b)
        )
    )
    (iter 1 f a next b)
)

(define (product-rec f a next b)
    (if (< b a)
        1
        (* (f a) (product-rec f (next a) next b))
    )
)


(define (accumulate combiner null-values term a next b)
    (define (iter res combiner null-values term a next b)
        (if (< b a)
            res
            (iter (combiner res (term a)) combiner null-values term (next a) next b)
        )
    )
    (iter null-values combiner null-values term a next b)
)

(define (accumulate-rec combiner null-values term a next b)
    (if (< b a)
        null-values
        (combiner (term a) (accumulate-rec combiner null-values term (next a) next b))
    )
)

(define (filter-accumulate combiner null-values term a next b filter)
    (define (iter res combiner null-values term a next b filter)
        (cond ((< b a) res)
        ((filter a) (iter (combiner res (term a)) combiner null-values term (next a) next b filter))
        (else (iter res combiner null-values term (next a) next b filter)))
    )
    (iter null-values combiner null-values term a next b filter)
)

(define (filter-accumulate-rec combiner null-values term a next b filter)
    (cond  ((< b a) null-values)
    ((filter a) (combiner (term a) (filter-accumulate combiner null-values term (next a) next b filter)))
    (else (filter-accumulate combiner null-values term (next a) next b filter)))
)

(define (f g)
    (g 2)
)