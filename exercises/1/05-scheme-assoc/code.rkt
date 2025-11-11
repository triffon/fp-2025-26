#lang racket

(define my-assoc
  (list
   (cons 'pesho 42)
   (cons 'gosho 26)
   (cons 'penka 128)
   (cons 3 56)))

(define (member? x l)
  (not (null? (filter (lambda (y) (equal? x y)) l))))

(define (assoc-keys l)
  (map car l))

(define (assoc-values l)
  (map cdr l))

(define (assoc-has? l k)
  (member? k (assoc-keys l)))

(define (replace-val-if pair k v)
  (if (equal? (car pair) k)
      (cons (car pair) v)
      pair))

(define (assoc-set l k v)
  (if (assoc-has? l k)
      (map (lambda (pair) (replace-val-if pair k v)) l)
      (cons (cons k v) l)))

(define (push-back l v)
  (if (null? l)
      (list v)
      (cons (car l) (push-back (cdr l) v))))

(define (assoc-set2 l k v)
  (if (null? l)
      (list (cons k v))
      (if (equal? (car (car l)) k)
          (cons (cons k v) (cdr l))
          (cons (car l) (assoc-set2 (cdr l) k v)))))

(define (assoc-set3 l k v)
  (if (assoc-has? l k)
      (map (lambda (pair) (replace-val-if pair k v)) l)
      (push-back l (cons k v))))