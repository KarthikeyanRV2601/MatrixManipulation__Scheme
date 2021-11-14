#lang racket

(define (get-matrix-row-n matrix n)
    '()
)

(define (swap matrix row1 row2 col)
  (let loop ((index 0))
    (cond ((>= index col) #t)
          ('())
          (else (loop (+ 1 index))))))