#lang racket
(provide validateDimensions validateSum-difference-Constraints multiplication-determinant-Constraints)

(define (validateDimensions n m)
    (and (> n 0) (> m 0))
)

(define (validateSum-difference-Constraints n1 m1 n2 m2)
    (and (= n1 n2) (= m1 m2)))

(define (multiplication-determinant-Constraints n m)
    (= n m)
)

;for inverse
;matrix det nonzero cond
;n=m
