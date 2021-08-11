#| 
This file contains ideas for implementing an array-like datastructure as procedures
|#


(define (initArray)
; Initialization of the array
    (define (array n)
; -1 is the argument for which the array returns the current length of the array
        (if (= n (- 1)) 0 #f)
    )
    array
)

(define (addArray array value)
; Add a new value at the end of the array and returns the new array
    (define (newArray n) 
        (cond ((= n (array -1)) value)
              ((= n -1) (+ (array -1) 1))
              (else (array n))
        )
    )
    newArray
)

(define (setArray array idx val)
; Procedure to set the value val at index idx of the array
; If the idx if out of range returns the array without changing anything
    (define (newArray n)
        (cond ((= idx n) val)
        (else (array n)))
    )
    (if (> idx (- (array -1) 1)) array
    newArray)
)

(define (removeArray array idx)
; Procedure to delete the element at the given idx
    (define (newArray n)
        (cond ((= n -1) (- (array -1) 1))
        ((< n idx) (array n))
        (else (array (+ n 1)))
        )
    )
    (if (> idx (- (array -1) 1)) array
    newArray)
)