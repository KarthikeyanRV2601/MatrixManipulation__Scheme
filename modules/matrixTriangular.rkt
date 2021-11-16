#lang racket
(provide triangular)
(require "./validationProcedures.rkt")


(define (triangleChooser choice)
  (if(= choice 1)
    (lambda (l item currentItem)
      ( let lower_rowelements(  
                ;list , item is bound item and currentItem is iterator
                (l l)
                (item item)
                (currentItem currentItem)
        )
        (if (isEmptyList l) '()
        (if (> currentItem item) (cons 0 (lower_rowelements (cdr l) item (+ currentItem 1)))
          (cons (car l)
          (lower_rowelements (cdr l) item (+ currentItem 1)))
          ))
      ))

    (lambda (l item currentItem)
    ( let upper_rowelements(  
                ;list , item is bound item and currentItem is iterator
                (l l)
                (item item)
                (currentItem currentItem)
        )
        (if (isEmptyList l) '()
      (if (< currentItem item) (cons 0 (upper_rowelements (cdr l) item (+ currentItem 1)))
        (cons (car l)
        (upper_rowelements (cdr l) item (+ currentItem 1)))
        ))
      ))
  )
)



(define (helperTriangle m i flag)
  (if (null? m) '()
    (cons 
    ((triangleChooser flag)(car m) i 0)
    (helperTriangle (cdr m) (+ i 1) flag))))

(define (triangular m flag)
    (helperTriangle m 0 flag))
