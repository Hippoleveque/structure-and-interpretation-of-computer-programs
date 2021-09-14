#|
Section 2.2.4 - Picture Language
|#


(define (flipped-pairs painter)
    (let ((painter2 (beside painter (rever painter))))
        (below painter2 painter2)
    )

)


(define (right-split painter n)
    (if (= 0 n)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller))
        )
    )
)


(define (up-split painter n)
    (if (= 0 n) 
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller                                                                                                    ))
        )
    )
)

(define (corner-split painter n)
    (let (up (up-split painter (- n 1))
         (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
              (beside (below painter top-left) (below bottom-right corner))
        )
    )
)

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz corner) corner)))
            (below half (flip-vert half))
        )
    )
)

(define (square-of-four tl tr bl br)
    (lambda(painter)
        (let ((up (beside (tl painter) (tr painter))) (down (beside (bl painter) (br painter))))

            (below down up)
        )
    )
)

(define (flipped-pairs painter)
    (let ((combiner (square-of-four identity flip-vert identity flip-vert))
        (combiner painter)
    )
)

(define (square-limit painter)
    (let ((combiner (square-of-four flip-horiz identity (lambda(p) (flip-vert (flip-horiz p))) flip-vert)))
        (combiner (corner-split painter n)
    )
)

(define (split combine-with-next combine-split)
    (define (helper painter n)
        (if (= 0 n)
            painter
            (let (smaller (helper painter (- n 1)))
                (combine-with-next painter (combine-split smaller smaller))
            )
        )
    )
    helper
)

(define right-split (split beside below))

(define up-split (split below beside))


(define (make-vect x y)
    (cons x y)
)

(define (xcor-vect vect)
    (car vect)
)

(define (ycor-vect vect)
    (cons vect)
)

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))
)

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))
)

(define (scale-vect v s)
    (make-vect (* s (xcor-vect v)) (* s (ycor-vect v)))
)

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2)
)

(define (origin-frame frame)
    (car frame)
)

(define (edge1-frame frame)
    (cadr frame)
)

(define (edge2-frame frame)
    (caddr frame)
)

(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2))
)

(define (origin-frame frame)
    (car frame)
)

(define (edge1-frame frame)
    (cadr frame)
)

(define (edge2-frame frame)
    (cddr frame)
)

(define (make-segment start end)
    (cons start end)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (frame-coord-map frame)
    (lambda(v)
        (add-vect 
            (origin-frame frame)
            (add-vect
                (scale-vect (edge1 frame) (xcor-vect v))
                (scale-vect (edge2 frame) (ycor-vect v))
            )
        )
    )
)

(define (segments->painter segment-list)
    (lambda(frame)
        (for-each
            (lambda(segment)
                (draw-line 
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))
                )
            )
        segment-list
        )
    )
)

(define outline (
    segments->painter 
        (list (make-segment 
                (make-vect 0 0)
                (make-vect 0 1)
               )
               (make-segment
                    (make-vect 0 1)
                    (make-vect 1 1)
                )
                (make-segment
                    (make-vect 1 1)
                    (make-vect 1 0)
                )
                (make-segment
                    (make-vect 1 0)
                    (make-vect 0 0)
                )
        )
)

(define x-outline (
    segments->painter
    (list (make-segment 
            (make-vect 0 0)
            (make-vect 1 1)
          )
          (make-segment
            (make-vect 0 1)
            (make-vect 1 0)
          )
    )
))

(define diamond-outline (
    segments->painter
    (list (make-segment
            (make-vect 0 0.5)
            (make-vect 0.5 1)
           )
           (make-segment
            (make-vect 0.5 1)
            (make-vect 1 0.5)
           )
           (make-segment
            (make-vect 1 0.5)
            (make-vect 0.5 0)
           )
           (make-segment
            (make-vect 0.5 0)
            (make-vect 0 0.5)
           )
    )
))


(define (transform-painter painter origin corner1 corner2)
    (lambda(frame)
        ((let m (frame-coord-map frame))
            (let (new-origin (m origin))
                (painter
                    (make-frame
                        new-origin
                        (sub-vect (m corner1) new-origin)
                        (sub-vect (m corner2) new-origin)
                    )
                )
            )
        )
    )
)

(define (flip-vert painter)
    (let ((origin (make-vect 0 1))
          (corner1 (make-vect 1 1))
          (corner2 (make-vect 0 0)))
        (transform-painter painter origin corner1 corner2)
    )
)

(define (shrink-to-upper-right painter)
    (let ((origin (make-vect 0.5 0.5))
        (corner1 (make-vect 1 0.5))
        (corner2 (make-vect 0.5 1)))
        (transform-painter painter origin corner1 corner2)
    )
)

(define (rotate-90 painter)
    (let ((origin (make-vect 1 0))
        (corner1 (make-vect 1 1))
        (corner2 (make-vect 0 0)))
        (transform-painter painter origin corner1 corner2)
    )
)

(define (besides painter1 painter2)
    (let ((origin1 (make-vect 0 0))
        (corner1 (make-vect 0.5 0))
        (corner2 (make-vect 1 0))
        (corner3 (make-vect 1 0))
        (corner4 (make-vect 0.5 1)))
        (let 
            ((paint-left (transform-painter painter1 origin1 corner1 corner2))
            (paint-right (transform-painter painter2 corner1 corner3 corner4)))
        )
        (lambda(frame) (paint-left frame) (paint-right frame))
    )
)


(define (flip-horiz painter)
    (let ((origin (make-vect 1 0))
        (corner1 (make-vect 0 0))
        (corner2 (make-vect 1 1)))
        (transform-painter painter origin corner1 corner2)
    )
)

(define (rotate-180 painter)
    (let ((origin (make-vect  1 1))
        (corner1 (make-vect 0 1))
        (corner2 (make-vect 1 0)))
        (transform-painter painter origin corner1 corner2)
    )
)

(define (rotate-270 painter)
    (let ((origin (make-vect 0 1))
        (corner1 (make-vect 0 0))
        (corner2 (make-vect 1 1)))
        (transform-painter painter origin corner1 corner2)
    )
)

(define (below painter1 painter2)
    (let ((origin1 (make-vect 0 0))
        (corner1 (make-vect 1 0))
        (corner2 (make-vect 0.5 0))
        (corner3 (make-vect 0.5 1))
        (corner4 (make-vect 1 0)))
        (let 
            ((paint-down (transform-painter painter2 origin1 corner1 corner2))
            (paint-up (transform-painter painter1 corner2 corner3 corner4)))
        )
        (lambda(frame) (paint-down frame) (paint-up frame))
    )
)

(define (below painter1 painter2)
    (rotate-90 (besides (rotate-270 painter1) (rotate-270 painter2))
)