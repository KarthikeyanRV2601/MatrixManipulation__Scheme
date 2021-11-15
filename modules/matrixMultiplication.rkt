#lang racket

(provide matrix-mul)


(define (list_dynamic_reducer operator list_instance)
  (let iter (
    ;defining accumulator to store the accumulated result
    (accumulator (car list_instance)) 
    ;defining list to reduce on
    (list_instance (cdr list_instance))
    )
    (if (null? list_instance) 
      accumulator
      (iter (operator accumulator (car list_instance)) (cdr list_instance)))))



(define (get_nth_matrixcol M n)
  (let iter (
    ;defining iterator
    (i (length M)) 
    ;a list to store the nth column of the matrix
    (result '()))
    (if (= i 0)
        result
        (iter (- i 1)
              (cons (get_list_element (get_list_element M (- i 1)) n) result)))))

(define (get_list_element list n)
  ;result stores the updated list at each iteration
  (let loop ((n n) (result list))
    (if (= n 0)
    (car result)
    (loop (- n 1) (cdr result)))))
(define (row-mul-operation operator)
    (lambda (N M) (map operator N M))
  )

(define (matrix-mul N M)
  (let row_wise_iterator (
    ;defining iterator 
    (i (length N)) 
    ;a list of lists that stores the resultant multiplied matrix
    (matrix_multiplied '()))
    
    (if (= i 0) 
      matrix_multiplied
      (row_wise_iterator (- i 1)
        (cons (let col_wise_iterator ((j (length (car M))) (constructed_row '()))
            (if (= j 0)
              constructed_row
              (col_wise_iterator (- j 1) 
              (cons 
              ;list reducer here calculates the sum of all elements in list
              (list_dynamic_reducer + (
                ;row mul operation returns a list obtained by  multiplicative mapping of the row and coloumn
                (row-mul-operation *)(get_list_element N (- i 1)) (get_nth_matrixcol M (- j 1)))) 
              constructed_row)
              )
            )
          ) 
          matrix_multiplied)))))