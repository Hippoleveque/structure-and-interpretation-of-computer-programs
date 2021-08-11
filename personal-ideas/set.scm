#| 
This file contains ideas for implementing a set-like datastructure 
as a procedure
|#

(define (initSet)
    (define (set value)
        #f
    )
    set
)

(define (addToSet set value)
    (define (newSet val)
        (if (= val value) #t (set val))
    )
    newSet
)

(define (removeToSet set value)
    (define (newSet val)
        (if (= val value) #f (set val))
    )
    (if (set value) newSet set
    )
)