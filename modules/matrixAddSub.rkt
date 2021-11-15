#lang racket
(provide matrix-sum-sub-operation)

(define (matrix-sum-sub-operation type)
  (if (= type 1)
    (lambda (x y) (map + (car x) (car y)))
    (lambda (x y) (map - (car x) (car y))))
  )

(provide matrix-sum-sub-main)

(define (matrix-sum-sub-main N M type)
  (let loop ((N N) (M M) (result '()))
    (if (or (null? N) (null? M))
        (reverse result)
        (loop (cdr N) 
              (cdr M)
              (cons ((matrix-sum-sub-operation type) N M) result))))
)