
#lang racket

(provide matrix-determinant)

(define (get-index pred l)
  (let loop ((index 0) (l l))
    (cond ((null? l) #f)
          ((pred (car l)) index)
          (else (loop (add1 index) (cdr l))))))

(define (swap l i1 i2)
  (let loop ((l l) (i1 (min i1 i2)) (i2 (max i1 i2)))
    (if (= 0 i1)
      (cons (list-ref l i2)
            (replace-index (cdr l) (sub1 i2) (car l)))
      (cons (car l)
            (loop (cdr l) (sub1 i1) (sub1 i2))))))

(define (replace-index l i v)
  (if (= i 0)
    (cons v (cdr l))
    (cons (car l) (replace-index (cdr l) (add1 i) v))))



(define (matrix-determinant m)
  (let ((index (get-index (lambda (r) (not (= 0 (car r)))) m)))
    (if (not index)
      0
      (let ((swap-multiplier (if (= index 0) 1 -1))
            (m-swapped (if (= index 0) m (swap m 0 index))))
        (let ((submatrix (determinant-helper m-swapped)))
          (if (null? submatrix)
            (caar m-swapped)
            (* swap-multiplier
                (caar m-swapped)
                (matrix-determinant submatrix))))))))


(define (determinant-helper m)
  (let ((first-row (car m)))
    (map (lambda (row)
            (let ((mult (/ (car row)
                          (car first-row))))
              (map (lambda (above current)
                    (- current (* mult above)))
                  (cdr first-row)
                  (cdr row))))
          (cdr m))))


