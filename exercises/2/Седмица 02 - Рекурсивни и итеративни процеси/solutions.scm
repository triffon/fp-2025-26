(define (sum-n n)
  (if (zero? n) 0
      (+ n (sum-n (- n 1)))))

(define (sum-n-iter n)
  (define (for i n result)
    (if (> i n) result
        (for (+ i 1) n (+ i result))))
  (for 0 n 0))

(define (reverse-n n)
  (define (reverse-iter current result)
    (if (zero? current) result
        (reverse-iter (quotient current 10)
                      (+ (remainder current 10)
                         (* result 10)))))
  (reverse-iter n 0))

(define (count-digits n)
  (cond ((< n 0) (count-digits (- n)))
        ((< n 10) 1)
        (else (+ 1 (count-digits (quotient n 10))))))

(define (count-digits-iter n)
  (define (while current result)
    (cond ((< current 0) (while (- current) result))
          ((zero? current) result)
          (else (while (quotient current 10) (+ 1 result)))))
  (while n 0))

(define (divisors-sum n)
  (define (for i n sum)
    (cond ((> i n) sum)
          ((zero? (remainder n i)) (for (+ i 1) n (+ sum i)))
          (else (for (+ i 1) n sum))))
  (for 1 n 0))

(define (decimal-to-binary n)
  (if (zero? n) 0
      (+ (remainder n 2)
         (* (decimal-to-binary (quotient n 2)) 10))))


(define (fibonacci n)
  (if (<= n 2) 1
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (fibonacci-linear n)
  (define (fibonacci-helper f1 f2 i)
    (if (> i n) f1
        (fibonacci-helper (+ f1 f2) f1 (+ i 1))))
  (if (<= n 2) 1
      (fibonacci-helper 1 1 3)))

(define (palindrome? n)
  (= n (reverse-n n)))

(define (fast-pow x n)
  (cond ((zero? n) 1)
        ((< n 0) (fast-pow (/ 1 x) (- n)))
        ((odd? n) (* x (fast-pow x (- n 1))))
        (else (fast-pow (* x x) (/ n 2)))))


(define (prime? n)
  (define (for i s)
    (or (> i s)
        (and (not (zero? (remainder n i)))
             (for (+ i 1) s))))
  (and (not (= n 1))
       (for 2 (sqrt n))))

(define (square? n)
  (define (binary-search begin end)
    (and (<= begin end)
         (let* ((middle (+ begin (quotient (- end begin) 2)))
               (middle-square (expt middle 2)))
           (or (= middle-square n)
               (and (< middle-square n)
                    (binary-search (+ middle 1) n))
               (and (> middle-square n)
                    (binary-search begin (- middle 1)))))))
  (binary-search 0 n))







