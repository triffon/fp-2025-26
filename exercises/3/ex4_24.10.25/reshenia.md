#lang racket

(define l (list 1 2 3 4 5))

(define l2 '(6 7 8 9 10 11))

(apply + (list 1 2 3))

(define (length2 l)
  (if (null? l) 0
      (+ 1 (length2 (cdr l)))))

(define (map2 f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map2 f (cdr lst)))))

(define (filter2 p? lst)
  (cond [(null? lst) '()]
        [(p? (car lst)) (cons (car lst) (filter p? (cdr lst)))]
        [else (filter p? (cdr lst))]))

(define (reverse3 lst)
  (if (null? lst) '()
      (append (reverse3 (cdr lst)) (list (car lst)))))

(define (at lst n)
  (cond [(null? lst) (error "undefined")]
        [(= n 0) (car lst)]
        [else (at (cdr lst) (- n 1))]))

(define (append-el l1 x)
  (if (null? l1) (list x)
      (cons (car l1) (append-el (cdr l1) x))))
  
(define (append2 l1 l2)
  (if (null? l1) l2
     (cons (car l1) (append2 (cdr l1) l2))))

(define (take lst n)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (car lst) (take (cdr lst) (- n 1)))]))

(define (drop lst n)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (cdr lst) (- n 1))]))

(define (all? p? lst)
  (if (null? lst) #t
      (and (p? (car lst)) (all? p? (cdr lst)))))

(define (all2? p? lst)
  (equal? (filter p? lst) lst))

(define (any? p? lst)
  (if (null? lst) #f
      (or (p? (car lst)) (any? p? (cdr lst)))))

(define (any2? p? lst)
  (not (null? (filter p? lst))))

(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (f (car lst1) (car lst2)) (zipWith f (cdr lst1) (cdr lst2)))
      ))

(define (zip lst1 lst2)
  (zipWith cons lst1 lst2))

(define (zipWith2 f lst1 lst2)
  (map (lambda (x) (f (car x) (cdr x))) (zip lst1 lst2)))

(define (sorted? lst)
  (cond [(null? lst) #t]
        [(null? (cdr lst)) #t]
        [else (and (<= (car lst) (cadr lst)) (sorted? (cdr lst)))]))

(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))

(define (length3 lst)
  (foldr (lambda (elem result) (+ 1 result)) 0 lst))

(define (all3? p? lst)
  (foldr (lambda (elem result) (and (p? elem) result)) #t lst))

(define (reverse2 lst)
  (foldl (lambda (res elem) (cons elem res)) '() lst))

(define (map3 f lst)
  (foldr (lambda (el res) (cons (f el) res)) '() lst))

(define (filter3 p? lst)
  (foldr (lambda (el res) (if (p? el) (cons el res) res)) '() lst))

(define (append3 l1 l2)
  (foldr cons l2 l1))
