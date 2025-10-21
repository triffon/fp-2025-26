#lang racket

; 1.1.
(/ (+ 100 50) (* 10 3))

; 1.2
(+ 1/3
   2.25
   (/ (- (expt 7 (modulo 60 7))
         (* 3 (quotient 21 6)))
      11))

; 1.3
(sqrt (+ (* 8 8) (* 15 15)))

; 2.1
(define pi 3.141592)

; 2.2
(define (circle-area r) (* pi r r))
(circle-area 3)

; 2.3
(define (cylinder-volume r h) (* (circle-area r) h))
(cylinder-volume 3 10)

; 3.1
(define (grade-label mark) (if (> mark 2) "Пръснах ги" "Скъсаха ме"))
(grade-label 2)
(grade-label 3)

; 3.2
(define (safe-divide a b) (if (= b 0) 0 (/ a b)))
(safe-divide 8 3)
(safe-divide 8 0)
; (/ 8 0) ; Throws an error

; 3.3
; With cond
(define (fizz-buzz x)
  (cond [(= (modulo x 15) 0) "FizzBuzz"]
        [(= (modulo x 3) 0) "Fizz"]
        [(= (modulo x 5) 0) "Buzz"]
        [else "Аз съм си число съм си"]
        )
)
(fizz-buzz 30)
(fizz-buzz 27)
(fizz-buzz 10)
(fizz-buzz 32)

; 3.4
(define (leap-year? y) (or (= (modulo y 400) 0) (and (= (modulo y 4) 0) (> (modulo y 100) 0))) )
(leap-year? 2000)
(leap-year? 2100)
(leap-year? 2008)

; 4.1
(define (sum-down-from n) (if (<= n 0) 0 (+ n (sum-down-from (- n 1)))))
(sum-down-from 11)

; 4.2
(define (string-repeat str n) (if (<= n 0) "" (string-append (string-repeat str (- n 1)) str)))
(string-repeat "Българи юнаци!" 3)

;4.3
(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(fib 7)

; 5.1
(define (to-base base x) (to-base-i base x 0))

(define (to-base-i base x i)
   (if (= x 0)
       0
       (+
        (* (expt 10 i) (remainder x base))
        (to-base-i base (quotient x base) (+ i 1)))))

(define (reverse x) (reverse-i x 0 0))

(define (reverse-i x pow acc)
   (if (= x 0)
       acc
       (reverse-i (quotient x 10) (+ pow 1) (+ (* acc 10) (remainder x 10)))))

(reverse 12201)

(define (palindrome? base x) (= (to-base base x) (reverse (to-base base x))))
(palindrome? 3 129)
(palindrome? 3 142)


(define (digit-parity-majority x)
    (cond
      [(> (digit-parity-counter x 0) 0) "even"]
      [(< (digit-parity-counter x 0) 0) "odd"]
      [else "equal"]))

(define (digit-parity-majority-2 x)
  (let ([parity-counter (digit-parity-counter x 0)])
    (cond
      [(> parity-counter 0) "even"]
      [(< parity-counter 0) "odd"]
      [else "equal"])))

(define
  (digit-parity-counter x cnt)
  (if (= (modulo x 10) 0)
      cnt
      (if (even? (modulo x 10))
          (digit-parity-counter (quotient x 10) (+ cnt 1))
          (digit-parity-counter (quotient x 10) (- cnt 1)))))

(digit-parity-majority 1)
(digit-parity-majority 2)
(digit-parity-majority 12)
(digit-parity-majority 12345678)
(digit-parity-majority 123456789)
(digit-parity-majority 123456786)
(digit-parity-majority -77444)