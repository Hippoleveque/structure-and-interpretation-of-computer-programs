#|
Section 2.1.3 - Exercise 2.1.4
|#

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) 
                   (+ (upper-bound x) (upper-bound y)))
)

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
)

(define (spans-zero x)
    (or (= 0.0 (lower-bound x)) (= 0.0 (upper-bound x)))
)


(define (div-interval x y)
    (if (spans-zero y)
        (error "The denumerator interval must not span 0")
        (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
    )   
)

(define (gte x y)
    (or (> x y) (= x y))
)


(define (mul-interval-cases x y)

    (let ((a lower-bound x)
          (b upper-bound x)
          (c lower-bound y)
          (d upper-bound y))
          (cond ((and
                    (< 0 a)
                    (< 0 b)
                    (< 0 c)
                    (< 0 d))
                    (make-interval (* d b) (* a c))
                )
            ((and 
                (< 0 a)
                (< 0 b)
                (< 0 c)
                (gte d 0))
                (make-interval (* a d) (* a c))
            )
            ((and
                (< 0 a)
                (< 0 b)
                (gte c 0)
                (gte d 0))
                (make-interval (* a d) (* b c))   
            )
            ((and 
                (< 0 a)
                (gte b 0)
                (< 0 c)
                (< 0 d))
                (make-interval (* b c) (* a c))
            )
            ((and 
                (< 0 a)
                (gte b 0)
                (gte c 0)
                (gte d 0))
                (make-interval (* a c) (* b d))
            )
            ((and 
                (gte a 0)
                (gte b 0)
                (< 0 c)
                (gte d 0))
            (make-inteval (* b c) (* b d))
            )
            ((and
                (gte a 0)
                (gte b 0)
                (gte c 0)
                (gte d 0))
            (make-interval (* a c) (* b d))
            )
            ((and
                (< 0 a)
                (gte b 0)
                (< 0 c)
                (gte d 0))
                (make-interval (min (* b c) (* a d)) (max (* a c) (* b d))))))
)


(define (make-interval a b)
    (cons a b)
)

(define (lower-bound x)
    (car x)
)

(define (upper-bound x)
    (cdr x)
)

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y)))
)

(define (make-interval-percent c p)
    (make-interval (- c (/ (* c p) 100.0)) (+ c (/ (* c p) 100.0)))
)

(define (percent x)
    (let ((center (+ (lower-bound x) (/ (- (upper-bound x) (lower-bound x)) 2))) )
        (* (/ width center) 100)
    )
)