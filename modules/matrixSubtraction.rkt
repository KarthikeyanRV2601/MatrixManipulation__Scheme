#lang racket
;matrix subtraction
(provide matrix-sub)

(define (matrix-sub N M)
  (let iter ((N N) (M M) (result '()))
    (if (or (null? N) (null? M))
        (reverse result)
        (iter (cdr N) 
              (cdr M)
              (cons (map - (car N) (car M)) result)))))
