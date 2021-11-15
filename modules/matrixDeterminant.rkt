
#lang racket

(provide determinant)

(define (switch-rows l i1 i2)
  (let loop ((l l) (i2 i2))
      (cons (list-ref l i2)
            (reconstruct (cdr l) (- i2 1) (car l)))))

(define (reconstruct l i v)
  (if (= i 0)
    (cons v (cdr l))
    (cons (car l) (reconstruct (cdr l) (- 1 i) v))))
    
(define (get-non-zero pred l)
  (let loop ((index 0) (l l))
    (cond ((null? l) -1)
          ((pred (car l)) index)
          (else (loop (+ 1 index) (cdr l))))))

(define (determinant matrix)
    (let ((index (get-non-zero (lambda (r) (not (= 0 (car r)))) matrix)))
        (if (= index -1)
        0
        (let ((sign (if (= index 0) 1 -1))
            (row-swapped (if (= index 0) matrix (switch-rows matrix 0 index))))
        (let ((submatrix (findSubMatrix row-swapped)))
            (if (null? submatrix)
                (caar row-swapped)
                (* sign (caar row-swapped) (determinant submatrix))
            ))
        ))
    )
)

(define (divide matrix element)
    (if (= element 0) '((0))
    (map (lambda (row)
            (cons (/ (car row) element) (cdr row))
        )matrix)
    )   
)

(define (makeSubmatrix matrix)
    (let ((top (car matrix)))
        (map (lambda (row)
                (let ((mult (car row)))
                    (map (lambda (firstRow curRow)
                            (- curRow  (* mult firstRow)))
                        (cdr top)
                        (cdr row))))
            (cdr matrix))))
            
(define (findSubMatrix matrix)
    (let ((unitMatrix (divide matrix (caar matrix))))
        (makeSubmatrix unitMatrix)
    )
)


