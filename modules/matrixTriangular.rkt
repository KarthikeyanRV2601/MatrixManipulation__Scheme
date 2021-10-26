#lang racket
(provide triangular)
(define (lower_rowelements l item currentItem)
    (if (null? l) '()
      (if (> currentItem item) (cons 0 (lower_rowelements (cdr l) item (+ currentItem 1)))
        (cons (car l)
        (lower_rowelements (cdr l) item (+ currentItem 1)))
        )))


(define (upper_rowelements l item currentItem)
    (if (null? l) '()
      (if (< currentItem item) (cons 0 (upper_rowelements (cdr l) item (+ currentItem 1)))
        (cons (car l)
        (upper_rowelements (cdr l) item (+ currentItem 1)))
        )))


(define (helperTriangle m i flag)
  (if (null? m) '()
    (cons 
      (if (= flag 1) (lower_rowelements (car m) i 0) (upper_rowelements (car m) i 0))
      (helperTriangle (cdr m) (+ i 1) flag))))

(define (triangular m flag)
    (helperTriangle m 0 flag))
