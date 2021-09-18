#|
Section 2.3.3
This file contains the procedures for the section 2.3.3 of the book
|#

(define (element-of-set? x S)
    (cond ((null? S) #f)
        ((= x (car S)) #t)
        (else (element-of-set? x (cdr S)))
    )
)

(define (adjoin-set x S)
    (if (element-of-set? x S)
        S
        (cons x S)
    )
)

(define (intersection-set S1 S2)
    (cond ((or (null? S1) (null? S2)) '())
    ((element-of-set? (car S1) S2) (cons (car S1) (intersection-set (cdr S1) S2)))
    (else (intersection-set (cdr S1) S2))
))


(define (union-set S1 S2)
    (cond ((null? S2) S1)
        ((element-of-set? (car S2) S1) (union-set S1 (cdr S2)))
        (else (union-set (adjoin-set (car S2) (cdr S2))))
    )
)

(define (element-of-set-duplicate? x S)
    ; O(n)
    (cond ((null? S) #f)
        ((= x (car S)) #t)
        (else (element-of-set? x (cdr S)))
    )
)

(define (adjoin-set-duplicate x S)
    ; O(1)
    (cons x S)
)

(define (intersection-set-duplicate S1 S2)
    (append S1 S2)
)

(define (element-of-set-ordered? x S)
    (cond ((null? S) #f)
        ((> x (car S)) #f)
        ((= x (car S)) #t)
        (else (element-of-set-ordered x (cdr S)))
    )
)

(define (intersection-set-ordered S1 S2)
    (cond ((or (null? S1) (null? S2)) '())
        ((= (car S1) (car S2)) (cons (car S1) intersection-set-ordered (cdr S1) (cdr S2)))
        ((> (car S1) (car S2)) (intersection-set-ordered S1 (cdr S2)))
        (else (intersection-set-ordered (cdr S1) S2))
)

(define (adjoin-set-ordered x S)
    (cond ((null? S) x)
        ((= x (car S)) S)
        ((> x (car S)) (cons x S))
        (else (cons (car S) (adjoin-set-ordered x (cdr S))))
    )
)

(define (union-set-ordered S1 S2)
    (cond ((null? S2) S1)
        ((= (car S1) (car S2)) (cons (car S1) (union-set-ordered S1 S2)))
        ((> (car S1) (car S2)) (cons (car S2) (union-set-ordered S1 (cdr S2))))
        (else (cons (car S1) (union-set-ordered (cdr S1) S2)))
    )
)

(define (entry-tree tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch)
)

(define (element-of-tree-set? x set)
    (cond ((null? set) #f)
        ((= x (entry-tree set)) #t)
        ((> x (entry-tree set)) (element-of-tree-set? x (right-branch set)))
        (else (element-of-tree-set? x (left-branch set)))
    )
)

(define (adjoin-tree-set x set)
    (cond ((null? set) x) 
        ((= x (entry-tree set)) set)
        ((> x (entry-tree set)) (list (entry-tree set) (left-branch set) (adjoin-tree-set x (right-branch set))))
        (else (list (entry-tree set) (adjoin-tee-set x (left-branch set)) (right-branch set)))
    )
)