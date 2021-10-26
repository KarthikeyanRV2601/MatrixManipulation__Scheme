#lang racket

(provide matrix-mul)

(define (reduce operator lst)
  (let iter ((result (car lst)) (lst (cdr lst)))
    (if (null? lst)
        result
        (iter (operator result (car lst)) (cdr lst)))))

(define (nth-element list n)
  (let iter ((n n) (result list))
    (if (= n 0)
        (car result)
        (iter (- n 1)
              (cdr result)))))

(define matrix-row nth-element)

(define (matrix-col M n)
  (let iter ((i (length M)) (result '()))
    (if (= i 0)
        result
        (iter (- i 1)
              (cons (nth-element (nth-element M (- i 1)) n) result)))))

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
