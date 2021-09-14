(define (square a)
    (* a a)
)

(define (even? a)
    (= 0 (remainder a 2))
)

(define (identity x) x)

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (inv x) (/ 1 x))

(define (positive? x) (< 0 x))

(define (negative? x) (> 0 x))

(define (average x y) (/ (+ x y) 2))
