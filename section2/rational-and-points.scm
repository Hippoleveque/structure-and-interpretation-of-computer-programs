#|
Section 2.1
This file contains all the data abstraction and procedures to manipulate rational numbers
|#

(define (make-rat n d)
    (let ((div (gcd n d)))
        (cond ((< 0 (* n d)) (cons (/ n div) (/ d div)))
            ((< n 0) (cons (/ n (- div)) (/ d (- div))))
            ((< d 0) (cons (/ n (- div)) (/ d (- div))))
        )
    )
)

(define (numer x)
    (car x)
)

(define (denom x)
    (cdr x)
)

(define (add-rat x y)
    (make-rat 
        (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
        (* (denom y) (denom x))
    )
)

(define (sub-rat x y)
    (make-rat
        (- (* (numer x) (denom y)) (* (numer y) (denom x)))
        (* (denom y) (denom x))
    )
)

(define (mul-rat x y)
    (make-rat
        (* (numer x) (numer y))
        (* (denom x) (denom y))
    )
)

(define (div-rat x y)
    (make-rat
        (* (numer x) (denom y))
        (* (denom x) (numer y))
    )
)

(define (equal-rat x y)
    (= (* (numer x) (denom y)) (* (denom x) (numer y)))
)

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
)

(define (gcd n d)
    (if (= 0 d)
        n
        (gcd d (remainder n d))
    )
)

(define (make-point x y)
    (cons x y)
)

(define (x-point point)
    (car point)
)

(define (y-point point)
    (cdr point)
)

(define (make-segment a b)
    (cons a b)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (midpoint-segment segment)
    (make-point 
        (/ (+ (car (car segment)) (car (cdr segment))) 2)
        (/ (+ (cdr (car segment)) (cdr (cdr segment))) 2)
    )
)

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
)

(define (make-rectangle p1 p2)
; definition with the lower-left upper-right points
    (cons p1 p2)
)

(define (heigth rec)
    (- (cdr (cdr rec)) (cdr (car rec)))
)

(define (width rec)
    (- (car (cdr rec)) (car (car rec)))
)

(define (make-rectangle-bis p1 w h)
; definition with lower-left point, width and heigth
    (cons p1 (cons w h))
)

(define (heigth-bis rec)
    (cdr (cdr rec))
)

(define (width-bis rec)
    (car (cdr rec))
)

(define (area rec)
    (* (width rec) (heigth rec))
)

(define (perimeter rec)
    (+ (* 2 (width rec)) (* 2 (heigth rec)))
)