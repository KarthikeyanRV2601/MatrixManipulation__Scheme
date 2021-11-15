#lang racket

(provide matrix-diagonal)

(define (diagonal-helper m i)
  (if (null? m) '()
    (cons (get_nth_item (car m) i 0)
      (diagonal-helper (cdr m) (+ i 1)))))

(define (get_nth_item l item currentItem)
    (if (null? l) '()
      (if (= currentItem item) (car l)
        (get_nth_item (cdr l) item (+ currentItem 1)))))

(define (matrix-diagonal m)
    (diagonal-helper m 0))
