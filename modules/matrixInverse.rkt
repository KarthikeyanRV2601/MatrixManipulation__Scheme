#lang racket

(provide inverse)
(require "./matrixDeterminant.rkt")
(require "./matrixTranspose.rkt")


(define (modulo a b)
  (- a (* b (floor (/ a b)))))

(define (getCofactor)
    (lambda (m p q n)
    (let outer-loop ((i 0) (result '()))
    (cond ((= i n) result)
        ((= i p) (outer-loop (+ i 1) result))
        (else (outer-loop (+ i 1)
                (append
                 result
                 (list
                 (let inner-loop ((j 0) (row '()))
                   (cond ((= j n) row)
                        ((= i p) (inner-loop (+ j 1) row))
                       ((= j q) (inner-loop (+ j 1) (append row '())))
                       (else (inner-loop (+ j 1) (append row (list (list-ref (list-ref m i) j)))))
                 ))))))))
                 
                 ))

(define (adjoint m n)
  (let outer-loop ((i 0) (result '()))
    (if (= i n)
        (matrix-transpose result)
        (outer-loop (+ i 1) 
                (append
                 result
                 (list
                 (let inner-loop ((j 0) (row '()))
                   (if (= j n)
                       row
                       (inner-loop (+ j 1) (append row (list (* (if (= (modulo (+ i j) 2) 0) 1 -1) (determinant ((getCofactor) m i j n))))))))
                 ))))))

(define (inverse m n)
    (let ((det (determinant m)) (adj (adjoint m n)) )
        (let outer-loop ((i 0) (res '()))
        (if (= i n) res
            (outer-loop (+ i 1)
                    (append
                    res
                    (list
                    (let inner-loop ((j 0) (row '()))
                    (if (= j n) row
                        (inner-loop (+ j 1) (append row (list (/ (list-ref (list-ref adj i) j) det))))
                    )))))))))