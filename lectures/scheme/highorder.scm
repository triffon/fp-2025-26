(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (fact n)      (accumulate * 1 1 n id 1+))
(define (pow x n)     (accumulate * 1 1 n (lambda (i) x) 1+))
(define (mystery x n) (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))
(define (exists? p? a b)
  (accumulate (lambda (x y) (or x y)) #f a b p? 1+))

(define (square x) (* x x))
(define (twice f) (lambda (x) (f (f x))))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))