#lang racket

(define (sum-digits sum x)
  (if (<= x 0)
      sum
      (sum-digits (+ sum (remainder x 10)) (quotient x 10))))

(define (is-special k x)
  (and (= 0 (remainder x k)) (= 0 (remainder (sum-digits 0 x) k))))

(define(count-specials k a b)
  (define (count-helper count k a b)
    (if (> a b)
        count
        (if (is-special k a)
            (count-helper (+ count 1) k (+ a 1) b)
            (count-helper count k (+ a 1) b))))
  (count-helper 0 k a b))

;(count-specials 3 3 9) ; 3
;(count-specials 5 10 100) ; 2
;(count-specials 8 100 200) ; 1
;(count-specials 15 1000 2000) ; 15

(define (count-digits n)
  (define (helper count n)
    (if (<= n 0)
        count
        (helper (+ count 1) (quotient n 10))))
  (helper 0 n))

(define (shift-left n)
  (+
   (*
    (remainder n (expt 10 (- (count-digits n) 1)))
    10)
   (quotient n (expt 10 (- (count-digits n) 1)))))

(define (lock-first-digits-in-shift n countOfDigitsLock)
  (if (< (count-digits(remainder n (expt 10 (- (count-digits n) countOfDigitsLock)))) (- (count-digits n) countOfDigitsLock))
  (+
   (*
    (quotient n (expt 10 (- (count-digits n) countOfDigitsLock)))
    (expt 10 (- (count-digits n) countOfDigitsLock)))
   (* (remainder n (expt 10 (- (count-digits n) countOfDigitsLock))) 10))
  (+
   (*
    (quotient n (expt 10 (- (count-digits n) countOfDigitsLock)))
    (expt 10 (- (count-digits n) countOfDigitsLock)))
   (shift-left (remainder n (expt 10 (- (count-digits n) countOfDigitsLock)))))))

(define (max-rot n)
  (define (helper n maxNumber operations)
    (if (= operations 0)
        maxNumber
        (helper (lock-first-digits-in-shift n (- (count-digits n) operations 1))
                (max (lock-first-digits-in-shift n (- (count-digits n) operations 1)) maxNumber)
                (- operations 1))))
  (helper n n (- (count-digits n) 1)))

;(max-rot 56789) ; 68957
;(max-rot 12490) ; 29140
;(max-rot 38458215) ; 85821534
;(max-rot 195881031) ; 988103115
;(max-rot 896219342) ; 962193428
;(max-rot 69418307) ; 94183076
;(max-rot 257117280) ; 571172802




