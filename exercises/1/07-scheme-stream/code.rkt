#lang racket

(define (nats-from x)
  (stream-cons x (nats-from (+ 1 x))))

(define nats (nats-from 0))

(define s12 (stream-cons 1 (stream-cons 2 empty-stream)))

(define (stream-take n s)
  (if (or (= n 0) (stream-empty? s))
      (list)
      (cons (stream-first s) (stream-take (- n 1) (stream-rest s)))))

(define (stream-map f s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (f (stream-first s)) (stream-map f (stream-rest s)))))

(define (stream-filter p s)
  (if (stream-empty? s)
      empty-stream
      (if (p (stream-first s))
          (stream-cons (stream-first s) (stream-filter p (stream-rest s)))
          (stream-filter p (stream-rest s)))))

(define (does-not-divide x y)
  (not (= 0 (remainder y x))))

(define (sieve numbers)
  (stream-cons
   (stream-first numbers)
   (sieve (stream-filter (lambda (x) (does-not-divide (stream-first numbers) x)) numbers))))

(define primes2 (sieve (nats-from 2)))

(define (iterate f x)
  (stream-cons x (iterate f (f x))))