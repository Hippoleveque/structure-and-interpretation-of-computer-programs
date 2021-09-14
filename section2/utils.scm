
(define (power a n)
    (power-iter 1 a n)
)


(define (power-iter res a n)
    (cond  ((= 0 n) res)
    ((= 0 (remainder n 2)) (power-iter res (square a) (/ n 2)))
    (else (power-iter (* res a) a (- n 1))))
)

(define (square x)
    (* x x)
)

(define (even x) (= 0 (remainder x 2)))

(define (fib n)
    (define (iter a b count)
        (if (= 0 count)
            b
            (iter (+ a b) a (- count 1))
        )
    )
    (iter 1 0 n)
)

(define (my-map proc seq)
    (if (null? seq) 
        ()
        (cons (proc (car seq)) (my-map proc (cdr seq)))
    )
)


(define (append seq1 seq2)
    (if (null? seq1)
        seq2
        (cons (car seq1) (append (cdr seq1) seq2))
    )
)

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
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

(define (reverse seq)
    (if (null? (cdr seq))
        (list (car seq))
        (append (reverse (cdr seq)) (list (car seq)))
    )
)

(define (reverse seq)
    (define (iter res rest)
        (if (null? rest)
            res
            (iter  (cons (car rest) res) (cdr rest))
        )
    )
    (iter () seq)
)

(define (filter predicate seq)
    (cond ((null? seq) ()) 
        ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))
    )
)


(define (enumerate-interval low high)
    (if (< high low)
        ()
        (cons low (enumerate-interval (+ low 1) high))
    )
)

(define (get-last seq)
    (if (null? (cdr seq)) (car seq) (last (cdr seq)))
)

(define (contains? el seq is-equal?)
    (cond ((null? seq) #f)
    ((is-equal? el (car seq)) #t)
    (else (contains? el (cdr seq) is-equal))
)

(define (always-true el) #t)