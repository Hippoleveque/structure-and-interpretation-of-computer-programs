#|
Section 2.2.3
|#


(define (sum-odd-squares tree)
    (cond ((null? tree) 0) 
        ((pair? tree) (+ (sum-odd-squares (car tree)) (sum-odd-squares (cdr tree))))
        ((even? tree) 0)
        (else (square tree))    
    )
)



(define (even-fibs n)
    (define (next k)
        (if (< n k) 
            ()
            (let ((f (fib k)))
                (if (even? f)
                    (cons f (next (+ k 1)))
                    (next (+ k 1))
                )
            )
        )
    )
    (next 0)
)


(define (filter predicate seq)
    (cond ((null? seq) ())
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))
    )
)

(define (accumulation op initial seq)
    (if (null? seq) 
        initial
        (op (car seq) (accumulation op initial (cdr seq)))
    )
)

(define (enumerate-interval low high)
    (if (< high low) 
        ()
        (cons low (enumerate-interval (+ low 1) high))
    )
)

(define (enumerate-tree tree)
    (cond ((null? tree) ())
        ((pair? tree) (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
        (else (list tree))
    )
)

(define (clean-sum-odd-squares tree)
    (accumulation 
        + 
        0 
        (map square (filter odd? (enumerate-tree tree)))
    )
)

(define (clean-even-fibs n)
    (accumulation
        cons
        ()
        (filter even? (map fib (enumerate-interval 0 n)))
    )
)

(define (list-fib-squares n)
    (accumulation
        cons
        ()
        (map square (map fib (enumerate-interval 0 n)))
    )
)

(define (product-of-squares-of-odd-elements sequence)
    (accumulation
        *
        1
        (map square (filter odd? sequence))
    )
)

(define (new-map p sequence)
    (accumulation 
        (lambda (x y) (cons (p x) y))
        ()
        sequence
    )
)

(define (new-append seq1 seq2)
    (accumulation
        cons
        seq2
        seq1
    )
)

(define (length sequence)
    (accumulation
        (lambda (x y) (+ 1 y))
        0
        sequence
    )
)

(define (horner-eval x sequence)
    (accumulation
        (lambda (a b) (+ a  (* x b)))
        0
        sequence
    )
)

(define (new-count-leave seq)
    (define (helper seq1 seq2)
        (if (pair? seq1)
            (+ (new-count-leave seq1) seq2)
            (+ 1 seq2)
        )
    )
    (accumulation
        helper
        0
        seq
    )
)

(define (other-count-leaves seq)
    (accumulation
        +
        0
        (map (lambda(x) 1) (enumerate-tree seq))
    )
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        ()
        (cons 
            (accumulation op init (map car seqs))
            (accumulate-n op init (map cdr seqs))
        )
    )
)


(define (dot-product v w)
    (accumulation 
        +
        0
        (map * v w)
    )
)

(define (my-dot-product v w)
    (accumulation
        +
        0 
        (accumulate-n * 1 (list v w))
    )
)

(define (matrix-*-vector m v)
    (map (lambda(x) (dot-product v x)) m)
)

(define (transpose mat)
    (accumulate-n cons () mat)
)

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map
            (lambda(x) (matrix-*-vector cols x))
            mat)
    )
)

(define (fold-right op init seq)
    (if (null? seq)
        init
        (op (car seq) (fold-right op init (cdr seq)))
    )
)


(define (fold-left op init seq)
    (define (iter res rest)
        (if (null? rest)
            res
            (iter (op (res) (car rest) (cdr rest))
        )
    )
    (iter init seq)
)

(define (reverse-fold-right seq)
    (fold-right
        (lambda (x y) (append y (list x)))
        ()
        seq
    )   
)

(define (reverse-fold-left seq)
    (fold-left
        (lambda (x y) (cons y x))
        ()
        seq
    )
)