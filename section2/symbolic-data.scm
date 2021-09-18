#|
Section 2.3.2
This file contains the procedures for the section 2.3.2 of the book
|#

(define (equals? list1 list2)
    (cond ((or (= list1 '()) (= list2 '())) (= list1 list2))
        ((= (car list1) (car list2)) (equals? (cdr list1) (cdr list2)))
        (else #f)
    )
)

(define (deriv expr var)
    (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
        ((product? expr) (make-sum (make-product (multiplier expr) (deriv (multiplicand expr) var)) (make-product (multiplicand expr) (deriv (multiplier expr) var))))
        ((exponentiation? expr) (make-product (make-product (exponent expr) (make-exponentiation (base expr) (- (exponent expr) 1))) (deriv (base expr) var)))
        (else (error "Unknown expression type --DERIV" expr))
    )
)

(define (variable? x)
    (symbol? x)
)

(define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y))
)

(define (make-sum x y)
    (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list '+ x y))
    )
)

(define (make-product x y)
    (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list '* x y))
    )
)

(define (=number? expr val)
    (and (number? expr) (= expr val))
)

(define (sum? x)
    (and (pair? x) (eq? (car x) '+))
)

(define (product? x)
    (and (pair? x) (eq? (car x) '*))
)

(define (addend s)
    (cadr s)
)

(define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (cons '+ (cddr s)))
)
    

(define (multiplier m)
    (cadr m)
)

(define (multiplicand m)
    (if (null? (cdddr m))
        (caddr m)
        (cons '* (cddr m)))
)

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**))
)

(define (base e)
    (cadr e)
)

(define (exponent e)
    (caddr e)
)

(define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
        ((=number? base 0) 0)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else (list '** base exponent))
    )
)