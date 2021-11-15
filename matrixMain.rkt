#lang racket



(require "./modules/matrixConstruct.rkt")
(require "./modules/matrixAddSub.rkt")
(require "./modules/matrixMultiplication.rkt")
(require "./modules/matrixDiagonal.rkt")
(require "./modules/matrixTranspose.rkt")
(require "./modules/matrixDeterminant.rkt")
(require "./modules/matrixTriangular.rkt")
(require "./modules/validationProcedures.rkt")


(display "enter n for matrix 1\n")
(define n1 (read))
(display "enter m for matrix 1\n")
(define m1 (read))



(if (validateDimensions n1 m1)
(display "\n")
((display "enter valid dimensions for matrix")(exit))
)

(display "\nEnter elements of first matrix 1\n")
(define matrix_N (construct-matrix n1 m1))

(display "enter n for matrix 2\n")
(define n2 (read))
(display "enter m for matrix 2\n")
(define m2 (read))


(if (validateDimensions n2 m2)
(display "\n")
((display "enter valid dimensions for matrix 2")(exit))
)

(display "\nEnter elements of second matrix\n")
(define matrix_M (construct-matrix m2 n2))



(display "\nmatrix_1\n")
matrix_N

(display "\nmatrix_2\n")
matrix_M

(display "\nMatrix addition\n")
(if (validateSum-difference-Constraints n1 m1 n2 m2)
    (display (matrix-sum-sub-main matrix_N matrix_M 1))
    (display "\nThese matrices cannot be added or subtracted\n")
)

(display "\nMatrix subtraction\n")
(if (validateSum-difference-Constraints n1 m1 n2 m2)
    (display (matrix-sum-sub-main matrix_N matrix_M 2))
    (display "\nThese matrices cannot be added or subtracted\n")
)


(display "\nMatrix 1 diagonal\n")
(display (matrix-diagonal matrix_N))

(display "\nMatrix 2 diagonal\n")
(display (matrix-diagonal matrix_M))

(display "\nMatrix 1 transpose\n")
(display (matrix-transpose matrix_N))

(display "\nMatrix 2 transpose\n")
(display (matrix-transpose matrix_M))

(display "\nMatrix multiplication\n")

(if (multiplication-determinant-Constraints n1 m2)
  (display (matrix-mul matrix_N matrix_M)) 
  (display "\nThese matrices cannot be multiplied\n")
)
 

(display "\nMatrix 1 determinant\n")
(if (multiplication-determinant-Constraints n1 m1)
  (determinant matrix_N)
  (display "\nDeterminant of this matrix cannot be calculated\n")
)

(display "\nMatrix 2 determinant\n")
(if (multiplication-determinant-Constraints n2 m2) 
     (determinant matrix_M)
  (display "\nDeterminant of this matrix cannot be calculated\n")
)


(display "\nLower triangular matrix\n")
(triangular matrix_N 1) ;1 means lower triangle

(display "\nUpper triangular matrix\n")
(triangular matrix_N 0) ;0 means upper triangle

