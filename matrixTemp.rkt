#lang racket

(display "enter n\n")
(define n (read))
(display "enter m\n")
(define m (read))


                 
(define (make-matrix n m)
  (let outter ((i n) (result '()))
    (if (= i 0)
        result
        (outter (- i 1) 
                (append
                 result
                 (list
                 (let inner ((j m) (row '()))
                   (if (= j 0)
                       row
                       (inner (- j 1) (append row (list (read))))))
                 ))))))




;matrix sum
(define (matrix-sum N M)
  (let iter ((N N) (M M) (result '()))
    (if (or (null? N) (null? M))
        (reverse result)
        (iter (cdr N) 
              (cdr M)
              (cons (map + (car N) (car M)) result)))))

;matrix subtraction

(define (matrix-sub N M)
  (let iter ((N N) (M M) (result '()))
    (if (or (null? N) (null? M))
        (reverse result)
        (iter (cdr N) 
              (cdr M)
              (cons (map - (car N) (car M)) result)))))

; matrix transpose
(define (matrix-transpose M)
  (if (null? (car M))
      '()
      (cons (map car M)
            (matrix-transpose (map cdr M)))))






; matrix multiplication

(define (reduce fun lst)
  (let iter ((result (car lst)) (lst (cdr lst)))
    (if (null? lst)
        result
        (iter (fun result (car lst)) (cdr lst)))))

(define (nth list n)
  (let iter ((n n) (result list))
    (if (= n 0)
        (car result)
        (iter (- n 1)
              (cdr result)))))

(define matrix-row nth)

(define (matrix-col M n)
  (let iter ((i (length M)) (result '()))
    (if (= i 0)
        result
        (iter (- i 1)
              (cons (nth (nth M (- i 1)) n) result)))))

(define (matrix-mul N M)
  (let rows ((i (length N)) (result '()))
    (if (= i 0)
        result
        (rows (- i 1)
              (cons
               (let cols ((j (length (car M))) (row '()))
                 (if (= j 0)
                     row
                     (cols
                      (- j 1)
                      (cons (reduce + (map *
                                           (matrix-row N (- i 1))
                                           (matrix-col M (- j 1))))
                            row))))
               result)))))


;diagonal elements
(define (nthItem l item currentItem)
    (if (null? l) '()
      (if (= currentItem item) (car l)
        (nthItem (cdr l) item (+ currentItem 1)))))

(define (diagPrivate m i)
  (if (null? m) '()
    (cons (nthItem (car m) i 0)
      (diagPrivate (cdr m) (+ i 1)))))

(define (diag m)
    (diagPrivate m 0))



;determinant

(require (lib "list.ss" "srfi" "1"))

  (provide det matrix-mul)

  (define (find-index pred l)
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

  ; O(n^3) where n is the height of m
  (define (det m)
    (let ((index (find-index (lambda (r) (not (= 0 (car r)))) m)))
      (if (not index)
        0
        (let ((swap-multiplier (if (= index 0) 1 -1))
              (m-swapped (if (= index 0) m (swap m 0 index))))
          (let ((submatrix (gaussian-submatrix m-swapped)))
            (if (null? submatrix)
              (caar m-swapped)
              (* swap-multiplier
                 (caar m-swapped)
                 (det submatrix))))))))

  ; Zeros entries under the top-left entry, by adding multiples of the first
  ; row. Returns the submatrix ommitting the first row and column. The top left
  ; entry must be nonzero. I couldn't really think of a good name, "gaussian
  ; submatrix" doesn't actually mean anything as far as I know.
  ; O(w*h) where w and h are the width and height of m
  (define (gaussian-submatrix m)
    (let ((first-row (car m)))
      (map (lambda (row)
             (let ((mult (/ (car row)
                            (car first-row))))
               (map (lambda (above current)
                      (- current (* mult above)))
                    (cdr first-row)
                    (cdr row))))
           (cdr m))))











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


(define matrix_N (make-matrix n m))
(define matrix_M (make-matrix m n))



matrix_N
matrix_M
(matrix-sum matrix_N matrix_M)
(matrix-sub matrix_N matrix_M)
(diag matrix_N)
(matrix-transpose matrix_N)
(matrix-mul matrix_N matrix_M)
(det matrix_N)
(triangular matrix_N 1) ;1 means lower triangle
(triangular matrix_N 0) ;0 means upper triangle

