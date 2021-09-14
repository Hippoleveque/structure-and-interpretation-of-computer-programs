#|
Section 2.2.3 - Nested mappings
|#


(define (construct-all-pairs n)
    (accumulate append () (map (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))
)

(define (flatmap proc seq)
    (accumulate append () (map proc seq))
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pairs-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)


(define (prime-sum-pairs n)
    (map 
        make-pairs-sum
        (filter 
            prime-sum?
            (flatmap 
                (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) 
                (enumerate-interval 1 n)
            )
        )
    )
)

(define (remove x seq)
    (filter (lambda (y) (not (= x y))) seq)
)

; Idea for generating all permutations of a set = for each elements, we append it at the beginning of all the subsets without this elements

; 1. get 

#|
1. For every elements get the sequences without this element
|#


(map (lambda (x) (remove x seq)) seq)


(map (lambda (x) (generate-all-subsets (remove x seq))) seq)


(define (permutations seq)
    (if (null? seq)
        (list ())
        (flatmap 
            (lambda (x) (map 
                (lambda (y) (cons x y)) 
                (permutations (remove x seq)))
            ) 
            seq
        )
    )
)


(define (unique-pairs n)
    (flatmap (lambda (i) (map (lambda(j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
)

(define (unique-pairs-asc n)
    (flatmap (lambda (i) (map (lambda(j) (list j i)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
)

(define (unique-triplets n)
    (flatmap (lambda(pair) (map (lambda(k) (cons k pair)) (enumerate-interval 1 (- (car pair) 1)))) (unique-pairs-asc n))
)

(define (check-triplets-sum triplet s)
    (= s (+ (car triplet) (cadr triplet) (caddr triplet)))
)

(define (all-triplets-sum n s)
    (define (check-triplets-sum triplet)
        (= s (+ (car triplet) (cadr triplet) (caddr triplet)))
    )
    (filter check-triplets-sum (unique-triplets n))
)



(define (check-rows pos)
    ; checks wether the last queen is in the same row as one of the previous
    (define (iter rest to-check)
        (cond ((null? (cdr rest)) #t)
        ((= to-check (car rest)) #f)
        (else (iter (cdr rest) to-check))
        )
    )
    (let ((last (get-last pos)))
        (iter pos last)
    )
)


(define (check-diag pos k)
    (define (iter rest pair-to-check curr-col)
        (let ((row (car pair-to-check)) (col (cadr pair-to-check)))
            (cond ((null? (cdr rest)) #t)
                ((= (- row (car rest)) (- col curr-col)) #f)
                ((= 0 (+ (- row (car rest)) (- col curr-col))) #f)
                (else (iter (cdr rest) pair-to-check (+ 1 curr-col)))
            )
        )
    )
    (let ((last (get-last pos)))
        (iter pos (list last k) 1)
    )
)


(define (queens board-size)
    ; returns all the possible solution for placing n queens on a board
    ; a solution is a list of numbers representing the row of the queen
    (define (queen-pos k)
    ; returns all the possible positions for the k first columns
        (define (check-queens pos)
        ; check rows and diag
            (and (check-rows pos) (check-diag pos k))
        )
        (if (= 0 k)
            ;(map (lambda(i) (list i)) (enumerate-interval 1 board-size))
            (list ())
            (filter check-queens 
                (flatmap 
                    (lambda(pos) 
                        (map (lambda(i) (append pos (list i)))
                        (enumerate-interval 1 board-size))
                    )
                    (queen-pos (- k 1))
                )
            )
        )
    )
    (queen-pos board-size)
)
