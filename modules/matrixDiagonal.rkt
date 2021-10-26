#lang racket

(provide matrix-diagonal)


(define (nthItem l item currentItem)
    (if (null? l) '()
      (if (= currentItem item) (car l)
        (nthItem (cdr l) item (+ currentItem 1)))))


(define (diagonal-helper m i)
  (if (null? m) '()
    (cons (nthItem (car m) i 0)
      (diagonal-helper (cdr m) (+ i 1)))))


(define (matrix-diagonal m)
    (diagonal-helper m 0))
