; Зад.2
; cond е друг условен израз и също е специална форма.
; Може и без else, ако искаме празна стойност като резултат
; За случаите може да използват (), както и [], няма значение за езика
; [] е по-четимо
(define (sign n)
  (cond [(< n 0) "negative")]
        [(> n 0) "positive")]
        [else "zero"]))

; Зад.3 
(define (divisible-by? d n)
  (zero? (remainder d n)))

; без условни оператори
(define (leap? year)
  (or (and (divisible-by? year 4)
           (not (divisible-by? year 100)))
      (divisible-by? year 400)))

; с условен оператор
(define (leap2? year)
(cond [(divisible-by? year 400) #t]
      [(divisible-by? year 100) #f]
      [(divisible-by? year 4) #t]
      [else #f]))

; Зад.4
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; Зад.5
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; Зад.6
(define (sum-interval a b)
  (if (> a b) ; Удобно дъно e "невалидния" интервал
      0
      (+ a (sum-interval (+ a 1) b))))

; Зад.6.1
(define (sum-interval3 a b)
  (if (> (+ a 2) b) 
      0
      (+ (+ a 1) (sum-interval3 (+ a 1) b))))

; На упражнение видяхме с дъно при (= (+ a 1) b), 
; но така с > разглеждаме всички случаи за a и b

; Зад.7
(define (count-digit n d)
  (if (= n 0) (if (= n d) 1 0)
      (+ (if (= (remainder n 10) d) 1 0) (count-digit (quotient n 10) d))
  ))

; Зад.8
(define (count-digit-interval2 d a b)
  (if (> a b) 0 
      (+ (count-digit a d) (count-digit-interval d (+ a 1) b))))

; Зад.9
; Понякога най-удобното дъно са едноцифрените числа,
; понякога е числото 0 - а понякога няма значение.
(define (length-number n)
  (if (< n 10)
       1
      (+ 1 (length-number (quotient n 10)))))

; Зад.10
(define (reverse-number n)
  (if (< n 10) n
      (+ (* (remainder n 10)
            (expt 10 (- (length-number n) 1)))
         (reverse-number (quotient n 10)))))

