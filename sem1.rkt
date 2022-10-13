#lang racket

(define (mymin a b) 

    (if(< a b) a b))

(define (inside? x a b)

    (and (<= a x) (<= x b)))


(define (myfunc a b)
    
        (/ (+ (* a a) (* b b)) 2))


(define (myfib n)

    (cond [(= n 0) 0]

          [(= n 1) 1]

          [else (+ (myfib (- n 1)) (myfib (- n 2)))]))


(define (fib-iter n)
  (define (helper prev cur i)
    (if (= i n)
        cur
        (helper cur (+ prev cur) (+ i 1))))
  (helper 1 0 0))

  ; define a function that sorst a list of numbers
(define (my-sort lst)
    (cond [(null? lst) '()]
            [else (insert (car lst) (my-sort (cdr lst)))]))
    
    ; define a function that inserts a number into a sorted list
(define (insert n lst)
    (cond [(null? lst) (list n)]
            [(<= n (car lst)) (cons n lst)]
            [else (cons (car lst) (insert n (cdr lst)))]))
            

; pi to the power of eulers constant
(define (myfunc2)
    (expt pi (exp 1)))
(myfunc2)

