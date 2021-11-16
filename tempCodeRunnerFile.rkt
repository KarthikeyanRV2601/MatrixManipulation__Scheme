#lang racket
(define lst '((1 2) (3 4)))
; [List-of [List-of Number]] -> [List-of Number]
; all elements of the matrix as a list
(define (print-matrix matrix)
  (apply append matrix))

; [List-of Nat] -> (void)
; prints the list with spaces in between
(define (print-list l)
  (match l
    ['() (void)]
    [(list e) (printf "~a" e)]
    [(cons f r) (printf "~a " f)
                (print-list (rest l))]))

; Matrix Sring -> (void)
; flattens and prints matrix to file
(define (flat-print-matrix matrix file)
  (with-output-to-file file
    (lambda () (print-list (print-matrix matrix)))))

(flat-print-matrix lst "output.txt")