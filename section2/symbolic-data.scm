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
        (else "Unknown expression type --DERIV" exp)
    )

)