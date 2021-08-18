#|
Section 1.2.5
This file contains the procedures to efficiently test for primality
And all exercises of section 1.2.5
|#

(define (gcd a b)
    (if (= 0 b) a
        (gcd b (remainder a b))
    )
)

(define (smallest-divisor n)
    (find-divisor n 2)
)

(define (find-divisor n test-for-divisor)
    (cond ((< n (square test-for-divisor)) n)
        ((= 0 (remainder n test-for-divisor)) test-for-divisor)
        (else (find-divisor n (next test-for-divisor)))
    )
)

(define (rel-prime? n m)
    (= 1 (gcd n m))
)


(define (prime? n)
    (= n (smallest-divisor n))
)

(define (expmod base exp-factor m)
    (cond ((= 0 exp-factor) 1)
        ((= 0 (remainder exp-factor 2)) (remainder (square (expmod base (/ exp-factor 2) m)) m))
        (else (remainder (* base (expmod base (- exp-factor 1) m)) m))
    )
)

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
    )
    (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime n times)
    (cond ((= 0 times) #t)
    ((fermat-test n) (fast-prime n (- times 1)))
    (else #f))
)

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
    (if (prime? n) (report-prime (- (runtime) start-time)))
)

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
)

(define (search-for-prime start-range end-range)
    (cond ((> start-range end-range) (newline)(display " ************ "))
          ((even? start-range) (search-for-prime (+ start-range 1) end-range))
          (else
            (timed-prime-test start-range)
            (search-for-prime (+ start-range 2) end-range)
          )
    )
)

(define (next a)
    (if (= a 2) 3 (+ a 2))
)

(define (full-fermat-test n)
    (full-fermat-test-iter 2 n)
)

(define (full-fermat-test-iter curr n)
    (define (try-it a)
        (= (expmod a n n) a)
    )
    (cond ((= curr n) #t)
    ((try-it curr) (full-fermat-test-iter (+ curr 1) n))
    (else #f))
)
