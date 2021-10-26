#lang racket

(provide construct-matrix)

(define (construct-matrix n m)
  (let outer-loop ((i n) (result '()))
    (if (= i 0)
        result
        (outer-loop (- i 1) 
                (append
                 result
                 (list
                 (let inner-loop ((j m) (row '()))
                   (if (= j 0)
                       row
                       (inner-loop (- j 1) (append row (list (read))))))
                 ))))))