#lang racket

(define f
  (λ (a b) (+ a b)))

(define (compose f g)
  (lambda (x)
    (g (f x))))

(define (5+ x)
  (+ x 5))

(define (3* x)
  (* x 3))

(define (S f g)
  (lambda (x)
    (f x (g x))))

(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (repeat f n)
  (lambda (x)
    (if (zero? n) x
        ((compose f (repeat f (- n 1))) x))))

(define (derive f)
  (let ([h 1e-6])
    (lambda (a)
      (/ (- (f (+ a h))
            (f a))
         h))))

(define (accumulate operation null-value begin end term next)
  (if (> begin end) null-value
      (operation (term begin)
                 (accumulate operation null-value (next begin) end term next))))

(define 1+
  (lambda (x) (+ 1 x)))

(define (sum-odd-squares a b)
  (accumulate + 0 a b
              (lambda (x)
                (if (odd? x)
                    (* x x)
                    0))
              1+))


(define (binomial n k)
  (accumulate * 1 1 k
              (lambda (i)
                (/ (+ n (- k) i) i))
              1+))

(define id
  (lambda (x) x))

(define (argmax f a b)
  (accumulate-iter (lambda (current result)
                (if (> (f current)
                       (f result))
                    current
                    result))
              a (1+ a) b id 1+))

(define (&& a b)
  (and a b))

(define (all? pred? a b)
  (accumulate && #t a b pred? 1+))

(define (sum-exponents a b)
  (accumulate + 0 a b (lambda (c)
                        (accumulate + 0 a b
                                    ((curry expt) c)
                                    1+))
              1+))

(define (accumulate-iter op nv begin end term next)
  (if (> begin end) nv
      (accumulate-iter op
                       (op (term begin) nv)
                       (next begin)
                       end term next)))





