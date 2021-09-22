#|
Section 2.3.3
This file contains the procedures for the section 2.3.3 of the book
|#

(define (make-leaf symbol weight)
    (list 'leaf symbol weight)
)

(define (leaf? object)
    (eq? (car object) 'leaf)
)

(define (symbol-leaf x)
    (cadr x)
)

(define (weight-leaf x)
    (caddr x)
)

(define (make-code-tree left right)
    (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right)))
)

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree) 
        (list (symbol-leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
    )
)

(define (decode bits tree)
    (define (decode-helper bits current-branch)
        (if (null? bits) 
            '()
            (let ((next-branch 
                (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch) 
                    (cons 
                        (symbol-leaf next-branch) 
                        (decode-helper (cdr bits) tree)
                    )
                    (decode-helper (cdr bits) next-branch)
                )
            )   
        )
    )
    (decode-helper bits tree)
)

(define (choose-branch bit branch)
    (cond ((= 0 bit) (left-branch branch))
        ((= 1 bit) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))
    )
)

(define (adjoin-set x S)
    (cond ((null? S) x)
        ((= (weight x) (weight (car S))) S)
        ((> (weigth x) (weight (car S))) (cons (car S) (adjoin-set x (cdr S))))
        (else (cons x S))
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set 
                (make-leaf (car pair) (cadr pair))
                (make-leaf-set (cdr pairs))
            )
        )
    )
)

(define sample-tree 
    (make-code-tree 
        (make-leaf 'A 4)
        (make-code-tree 
            (make-leaf 'B 2)
            (make-code-tree
                (make-leaf 'D 1)
                (make-leaf 'C 1)
            )
        )
    )   
)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0) )

(define (encode message tree)
    (define (encode-helper message current-branch)
        (cond ((null? message) '())
            ((leaf? current-branch) (encode-helper (cdr message) tree))
            ((memq (car message) (symbols (left-branch current-branch))) (cons 0 (encode-helper message (left-branch current-branch))))
            (else (cons 1 (encode-helper message (right-branch current-branch))))          
        )
    )
    (encode-helper message tree)
)

(define (encode-book message tree)
    (if (null? message)
        '()
        (append 
            (encode-symbol (car message) tree)
            (encode-book (cdr message) tree)
        )
    )
)

(define (encode-symbol symbol tree)
    (cond ((leaf? tree) 
            (if (memq symbol (symbols tree))
                '()
                (error "bad symbol -- ENCODE-SYMBOL" (list symbol (symbols tree)))
            )
          )
        ((memq symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        (else (cons 1 (encode-symbol symbol (right-branch tree))))
    )
)

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define (successive-merge ordered-pairs)
    (cond ((null? ordered-pairs) '())
        (())
    )

    ( ( = 1 (length ordered-pairs ))
        (car ordered-pairs)
        ()
    )
)