(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))

(fact 4)

(define (facti n)
  (for n 1 1))

(define (for n r i)
  (if (<= i n)
      (for n (* r i) (+ i 1))
      r))

(facti 4)

(define (qpow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (qpow x (- n))))
        ((even? n) (qpow (qpow x (quotient n 2)) 2))
        (else (* x (qpow x (- n 1))))))

;; (qpow 2 2) --> (qpow (qpow 2 1) 2) -> (qpow 2 2) --> ...

(define (qpow x n)
  (define (sq x) (* x x))
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (qpow x (- n))))
        ((even? n) (sq (qpow x (quotient n 2))))
        (else (* x (qpow x (- n 1))))))
