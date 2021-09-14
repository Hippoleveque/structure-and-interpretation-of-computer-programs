#|
Section 2.2.1
|#


(define (list-ref items n)
    (if (= 0 n)
        (car items)
        (list-ref (cdr items) (- n 1))
    )
)

(define (length items)
    (define (length-iter items len) 
        (if (null? items)
            len
            (length-iter (cdr items) (+ len 1))
        )
    )
    (length-iter items 0)
)

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (last-pair items)
    (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))
    )
)

(define (reverse alist)
    (define (reverse-iter items res)
        (if (null? items)
            res
            (reverse-iter (cdr items) (cons (car items) res))
        ) 
    )
    (reverse-iter alist ())
)

(define (cc amount coin-values)
    (cond ((= 0 amount) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else (+ (cc amount (except-first-denomination coin-values)) (cc (- amount (first-denomination coin-values)) coin-values))))
)

(define (except-first-denomination coin-values)
    (cdr coin-values)
)

(define (first-denomination coin-values)
    (car coin-values)
)

(define (no-more? coin-values)
    (null? coin-values)
)

(define (same-parity x . y)
    (define (iter res init alist)
        (let ((parity (remainder init 2)))
            (cond ((null? alist) res)
            ((= parity (remainder (car alist) 2)) (iter (append res (cons (car alist) ())) init (cdr alist)))
            (else (iter res init (cdr alist)))            
            )
        )
    )
    (iter (cons x ()) x y)
)

(define (same-parity-bis x . y)
    (define (iter parity alist)
        (cond ((null? alist) ())
        ((= parity (remainder (car alist) 2)) (cons (car alist) (iter parity (cdr alist)))) 
        (else (iter parity (cdr alist))))
    )
    (let ((parity (remainder x 2)))
        (cons x (iter parity y))
    )
)

(define (scale-list items factor)
    (if (null? items)
        ()
        (cons (* (car items) factor) (scale-list (cdr items) factor))
    )
)

(define (my-map proc items)
    (if (null? items)
        ()
        (cons (proc (car items)) (my-map proc (cdr items)))
    )
)


(define (square-list items)
    (if (null? items)
        ()
        (cons (square (car items))(square-list (cdr items)))
    )
)


(define (square-list-map items)
    (my-map square items)
)

(define (for-each proc items)
    
    (if (null? items)
        (newline)
        (and (proc (car items))
        (for-each proc (cdr items)))
    )
)

(define (show x)
    (newline)
    (display x)
)

(define (count-leave tree)
    (cond ((null? tree) 0)
        ((pair? tree) (+ (count-leave (car tree)) (count-leave (cdr tree))))
        (else 1)
    )
)

(define (deep-reverse items)
    (define (iter res alist)
        (cond ((null? alist) res)
        ((pair? (car alist)) (iter (cons (deep-reverse (car alist)) res) (cdr alist)))
        (else (iter (cons (car alist) res) (cdr alist)))
        )
    )
    (iter () items)
)

(define (count-leave tree)
(cond ((null? tree) 0)
    ((pair? tree) (+ (count-leave (car tree)) (count-leave (cdr tree))))
    (else 1)
)
)


(define (fringe tree)
    (cond ((null? tree) ())
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))
    )
)


(define (make-mobile left right)
    (list left right)
)

(define (make-branch len structure)
    (list len structure)
)

(define (left-branch mobile)
    (car mobile)
)

(define (right-branch mobile)
    (cadr mobile)
)

(define (branch-length branch)
    (car branch)
)

(define (branch-structure branch) 
    (cadr branch)
)

(define (total-weight mobile)
    (define (weight branch)
        (let ((struct (branch-structure branch)))
            (if (pair? struct)
                (total-weight struct)
                struct
            )
        )
    )
    (+ (weight (left-branch mobile)) (weight (right-branch mobile)))
)

(define (balanced? mobile)
; Let's defined a balance branch = all mobiles are balanced
    (define (balance-branch branch)
        (let ((struct (branch-structure branch)))
            (if (pair? struct)
                (balanced? struct)
                #t 
            )
        )
    )
    (define (torque branch)
        (define (weight branch)
            (let ((struct (branch-structure branch)))
                (if (pair? struct)
                    (total-weight struct)
                    struct
                )
            )
        )
        (* (branch-length branch) (weight branch))
    )

    (let ((left (left-branch mobile))
         (right (right-branch mobile)))
        (and (balance-branch left) (balance-branch right)
            (= (torque left) (torque right))
        )
    )
)

(define (scale-tree tree factor)
    (cond ((null? tree) ())
        ((pair? tree) (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
        (else (* tree factor))
    )
)

(define (scale-tree-map tree factor)
    (define (helper sub-tree)
        (cond ((null? sub-tree) ())
            ((pair? sub-tree) (scale-tree sub-tree factor))
            (else (* sub-tree factor))
        )
    )
    (my-map helper tree)
)

(define (square-tree tree)
    (cond ((null? tree) ())
    ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
    (else (square tree))
    )
)

(define (square-tree-map tree)
    (define (helper sub-tree)
        (cond ((null? sub-tree) ())
            ((pair? sub-tree) (square-tree sub-tree))
            (else (square sub-tree))
        )
    )
    (my-map helper tree)
)

(define (tree-map proc tree)
    (cond ((null? tree) ())
        ((pair? tree) (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))
        (else (proc tree))
    )
)

(define (tree-map-bis proc tree)
    (define (helper sub-tree)
        (cond ((null? sub-tree) ())
        ((pair? sub-tree) (tree-map-bis proc sub-tree))
        (else (proc sub-tree))
        )
    )
    (my-map helper tree)
)

(define (generate-all-subsets alist)
; we take the first elements and the final set will be the all subsets without this element + this element + all the (other subsets + it)
    (if (null? alist) 
        (list ())
        (let ((subsets (generate-all-subsets (cdr alist))))
            (append (map (lambda(m) (append (list (car alist)) m)) subsets) subsets)
        )  
    )
)