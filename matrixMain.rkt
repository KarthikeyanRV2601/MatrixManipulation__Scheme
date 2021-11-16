#lang racket



(require "./modules/matrixConstruct.rkt")
(require "./modules/matrixAddSub.rkt")
(require "./modules/matrixMultiplication.rkt")
(require "./modules/matrixDiagonal.rkt")
(require "./modules/matrixTranspose.rkt")
(require "./modules/matrixDeterminant.rkt")
(require "./modules/matrixTriangular.rkt")
(require "./modules/matrixInverse.rkt")
(require "./modules/validationProcedures.rkt")

(display "\n-----------------------------------\n")
(display "enter number of rows for matrix 1\n")
(define n1 (read))
(display "enter number of columns for matrix 1\n")
(define m1 (read))
(display "\n-----------------------------------\n")


(if (validateDimensions n1 m1)
(display "\n")
((display "enter valid dimensions for matrix")(exit))
)

(display "\nEnter elements of first matrix 1\n")
(define matrix_N (construct-matrix n1 m1))

(display "\n-----------------------------------\n")
(display "enter number of rows for matrix 2\n")
(define n2 (read))
(display "enter number of columns for matrix 2\n")
(define m2 (read))
(display "\n-----------------------------------\n")

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

(display "\n-----------------------------------\n")
(display "\nMatrix addition\n")
(if (validateSum-difference-Constraints n1 m1 n2 m2)
    (display (matrix-sum-sub-main matrix_N matrix_M 1))
    (display "\nThese matrices cannot be added or subtracted\n")
)
(display "\n-----------------------------------\n")

(display "\nMatrix subtraction\n")
(if (validateSum-difference-Constraints n1 m1 n2 m2)
    (display (matrix-sum-sub-main matrix_N matrix_M 2))
    (display "\nThese matrices cannot be added or subtracted\n")
)
(display "\n-----------------------------------\n")


(display "\nMatrix 1 diagonal\n")
(display (matrix-diagonal matrix_N))
(display "\n-----------------------------------\n")


(display "\nMatrix 2 diagonal\n")
(display (matrix-diagonal matrix_M))
(display "\n-----------------------------------\n")

(display "\nMatrix 1 transpose\n")
(display (matrix-transpose matrix_N))
(display "\n-----------------------------------\n")

(display "\nMatrix 2 transpose\n")
(display (matrix-transpose matrix_M))
(display "\n-----------------------------------\n")

(display "\nMatrix multiplication\n")

(if (multiplication-determinant-Constraints n1 m2)
  (display (matrix-mul matrix_N matrix_M)) 
  (display "\nThese matrices cannot be multiplied\n")
)
(display "\n-----------------------------------\n")
 

(display "\nMatrix 1 determinant\n")
(if (multiplication-determinant-Constraints n1 m1)
  (determinant matrix_N)
  (display "\nDeterminant of this matrix cannot be calculated\n")
)
(display "\n-----------------------------------\n")

(display "\nMatrix 2 determinant\n")
(if (multiplication-determinant-Constraints n2 m2) 
     (determinant matrix_M)
  (display "\nDeterminant of this matrix cannot be calculated\n")
)
(display "\n-----------------------------------\n")

(display "\nLower triangular matrix\n")
(triangular matrix_N 1) ;1 means lower triangle
(display "\n-----------------------------------\n")


(display "\nUpper triangular matrix\n")
(triangular matrix_N 0) ;0 means upper triangle
(display "\n-----------------------------------\n")



(display "\nInverse of a matrix 1\n")
(if (inverse-condition matrix_N n1 m1)
  (display (inverse matrix_N n1))
  (display "\nInverse of matrix 1 cannot be calculated\n")
)
(display "\n-----------------------------------\n")


(display "\nInverse of a matrix 2\n")
(if (inverse-condition matrix_M n2 m2)
  (display (inverse matrix_M n2))
  (display "\nInverse of matrix 2 cannot be calculated\n")
) 
(display "\n-----------------------------------\n")