 ;Зад. 1
(define (fact-iter n)
  (define (iter k prod)
    (if (>= k n) prod
        (iter (+ k 1) (* (+ k 1) prod)))
    )
  (iter 0 1)
  )

(define (fact-iter2 n)
  (define (iter k prod)
    (if (<= k 0) prod
        (iter (- k 1) (* k prod)))
    )
  (iter n 1)
  )

; Зад. 2
(define (fib-iter n)
  (define (iter k k1 k2)
    (if (>= k n) k2
    (iter (+ k 1) (+ k1 k2) k1))

    )
  (iter 0 1 0)
  )
; Зад. 3
(define (interval-sum a b)
  (define (iter a b res)
    (if (> a b) res
        (iter (+ a 1) b (+ res a))
  )) (iter a b 0))

; Зад. 4
(define (pow-i x n)
  (define (iter k acc)
    (if (zero? k) acc
        (iter (- k 1) (* x acc))))
  (iter n 1))

; Зад. 5
(define (number-length n)
  (define (iter n l)
    (if (< n 10) l
        (iter (quotient n 10) (+ l 1))))
  (iter n 1))

; Зад. 6
(define (reverse-digits n)
  (define (iter n rev)
    (if (< n 10) (+ (* rev 10) n)
        (iter (quotient n 10) (+ (* rev 10) (remainder n 10)))))
        (iter n 0))

; Зад. 7
(define (to-binary n)
  (if (< n 2) n
      (+ (remainder n 2) (* 10 (to-binary (quotient n 2))))))

(define (to-binary-i n)
  (define (iter n p acc)
    (if (< n 2) acc
       (iter (quotient n 2) (* p 10) (+ acc (* p (remainder n 2))) 
    )
  )) (iter n 0 0))

; Зад. 8 

(define (to-decimal b)
  (if (< b 10) b
      (+ (remainder b 10) (* 2 (to-decimal (quotient b 10))))))

(define (to-decimal-i n)
  (define (iter n p acc)
    (if (= n 0) acc
       (iter (quotient n 10) (* p 2) (+ acc (* p (remainder n 10))) 
    )
  )) (iter n 1 0))

; Заад. 9

(define (prime? n)
  (define (iter n i res)
    (if (> i (/ n 2)) res
        (iter n (+ i 1) (and res (not (zero? (remainder n i)))))))
  (iter n 2 #t)
  )

