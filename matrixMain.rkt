#lang racket



(require "./modules/matrixConstruct.rkt")
(require "./modules/matrixAddition.rkt")
(require "./modules/matrixSubtraction.rkt")
(require "./modules/matrixMultiplication.rkt")
(require "./modules/matrixDiagonal.rkt")
(require "./modules/matrixTranspose.rkt")
(require "./modules/matrixDeterminant.rkt")
(require "./modules/matrixTriangular.rkt")

(display "enter n for matrix 1\n")
(define n1 (read))
(display "enter m for matrix 1\n")
(define m1 (read))
(display "\nEnter elements of first matrix\n")
(define matrix_N (construct-matrix n1 m1))

(display "enter n for matrix 2\n")
(define n2 (read))
(display "enter m for matrix 2\n")
(define m2 (read))
(display "\nEnter elements of second matrix\n")
(define matrix_M (construct-matrix m2 n2))



(display "\nmatrix_1\n")
matrix_N

(display "\nmatrix_2\n")
matrix_M

(display "\nMatrix addition\n")
(matrix-sum matrix_N matrix_M)

(display "\nMatrix addition\n")
(matrix-sub matrix_N matrix_M)

(display "\nMatrix diagonal\n")
(matrix-diagonal matrix_N)

(display "\nMatrix transpose\n")
(matrix-transpose matrix_N)

(display "\nMatrix multiplication\n")
(matrix-mul matrix_N matrix_M)

(display "\nMatrix determinant\n")
(matrix-determinant matrix_N)

(display "\nLower triangular matrix\n")
(triangular matrix_N 1) ;1 means lower triangle

(display "\nUpper triangular matrix\n")
(triangular matrix_N 0) ;0 means upper triangle

