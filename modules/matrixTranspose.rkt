#lang racket


(provide matrix-transpose)
; matrix transpose
(define (matrix-transpose M)
  (if (null? (car M))
      '()
      (cons (map car M)
            (matrix-transpose (map cdr M)))))


