#lang racket


(provide matrix-sum)
;matrix sum

(define (matrix-sum N M)
  (let loop ((N N) (M M) (sumResult '()))
    (if (or (null? N) (null? M))
        (reverse sumResult)
        (loop (cdr N) 
              (cdr M)
              (cons (map + (car N) (car M)) sumResult)))))



