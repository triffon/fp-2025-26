#lang racket

(define my-list (list 1 2 3 4 42 26))

(define (square x) (* x x))
(define (cube x) (* x x x))

; (define (square-all l)
;   (if (null? l)
;       (list)
;       (cons (square (car l)) (square-all (cdr l)))))

; (define (cube-all l)
;   (if (null? l)
;       (list)
;       (cons (cube (car l)) (cube-all (cdr l)))))

(define (my-map f l)
  (if (null? l)
      (list)
      (cons (f (car l)) (my-map f (cdr l)))))

(define (square-all l) (my-map square l))
(define (cube-all l) (my-map cube l))

(define (my-filter p? l)
  (if (null? l)
      (list)
      (if (p? (car l))
          (cons (car l) (my-filter p? (cdr l)))
          (my-filter p? (cdr l)))))

(define (my-foldr op init l)
  (if (null? l)
      init
      (op (car l) (my-foldr op init (cdr l)))))

(define (sum l) (my-foldr + 0 l))

(define (prod l) (my-foldr * 1 l))

(define (andf x y) (and x y))

(define (forall? p l)
  (my-foldr (lambda (x y) (and x y)) #t (my-map p l)))

(define (exists? p l)
  (my-foldr (lambda (x y) (or x y)) #f (my-map p l)))

(define (exists2? p l)
  (not (forall? (lambda (x) (not (p x))) l)))

(define (my-member? x l)
  (exists? (lambda (y) (equal? x y)) l))

(define (check-unique x l)
  (if (null? l)
      (list x)
      (if (equal? x (car l))
          l
          (cons x l))))

(define (unique l)
  (foldr check-unique (list) l))