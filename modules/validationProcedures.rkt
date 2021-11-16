#lang racket

(require "./matrixDeterminant.rkt")
(provide validateDimensions validateSum-difference-Constraints multiplication-determinant-Constraints inverse-condition isEmptyList)

(define (validateDimensions n m)
    (and (> n 0) (> m 0))
)

(define (validateSum-difference-Constraints n1 m1 n2 m2)
    (and (= n1 n2) (= m1 m2))
)

(define (multiplication-determinant-Constraints n m)
    (= n m)
)

(define (inverse-condition matrix n m)
    (and (not (= (determinant matrix) 0)) (multiplication-determinant-Constraints n m))
)

(define (isEmptyList listPassed)
    (null? listPassed)
)
