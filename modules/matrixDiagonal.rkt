#lang racket
(require "./validationProcedures.rkt")
(provide matrix-diagonal)

(define (diagonal-helper m i)
  (if (isEmptyList m) '()
    (cons (item_from_list (car m) i 0)
      (diagonal-helper (cdr m) (+ i 1)))))

(define (item_from_list l requiredItem currentItem)
    (if (isEmptyList l) '()
      (if (= currentItem requiredItem) (car l)
        (item_from_list (cdr l) requiredItem (+ currentItem 1)))))

(define (matrix-diagonal m)
    (diagonal-helper m 0))
