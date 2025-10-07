(define (^2 n)
  (expt n 2))

(define (distance x1 y1 x2 y2)
  (sqrt (+ (^2 (- x1 x2))
           (^2 (- y1 y2)))))

(define (super-number a b c)
  (let* ((max-n (max a b c))
        (min-n (min a b c))
        (middle (+ a b c (- max-n) (- min-n))))
    (+ (* max-n min-n) middle)))

(define (reverse-3-digit-number n)
  (+ (quotient n 100)
     (* (remainder (quotient n 10) 10) 10)
     (* (remainder n 10) 100)))

(define (divisible-by? n d)
  (zero? (remainder n d)))

(define (leap? year)
  (or (divisible-by? year 400)
      (and (divisible-by? year 4)
           (not (divisible-by? year 100)))))

(define (factorial n)
  (if (zero? n) 1
      (* n (factorial (- n 1)))))

(define (interval-sum begin end)
  (if (= begin end) begin
      (+ begin (interval-sum (+ begin 1) end))))

(define (length-number n)
  (if (< n 10) 1
      (+ 1 (length-number (quotient n 10)))))

(define (reverse-number n)
  (if (< n 10) n
      (+ (* (remainder n 10)
            (expt 10 (- (length-number n) 1)))
         (reverse-number (quotient n 10)))))
  
  






