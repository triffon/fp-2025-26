(define (map f l)
  (foldr (lambda (el res) (cons (f el) res)) '() l))

(define (filter p? l)
  (foldr (lambda (el res) (if (p? el) (cons el res) res)) '() l))

(define (reverse lst)
  (foldl (lambda (res elem) (cons elem res)) '() lst))

(define (min-l l)
  (foldr min (car l) l)) ; (apply min l)? 

(define (member? x l)
  (foldr (lambda (el res) (or (equal? el x) res)) #f l))

(define (unique l)
  (foldr (lambda (el res) (if (member? el res) res (cons el res))) '() l))

(define (union l1 l2)
  (unique (append l1 l2)))

(define (intersection l1 l2)
  (filter (lambda (x) (member? x l2)) l1))

(define (insert x l)
  (if (null? l) (list x)
      (if (< x (car l)) (cons x l) (cons (car l) (insert x (cdr l))))))

(define (insertion-sort l)
  (foldr (lambda (el res) (insert el res)) '() l))

(define (find-interval . xs)
  (- (apply max xs) (apply min xs)))

(define (comp-lex a b)
  (if (string<? a b) a b))

(define (min-lex x . xs)
  (if (null? xs) x
      (comp-lex x (apply min-lex xs))))

(define (cart-prod lst1 lst2)
  (apply append
         (map (lambda (x)
                (map (lambda (y) (cons x y))
                     lst2))
              lst1)))

(define (succ x) (+ x 1))
(define (^2 x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (compose-all l)
  (cond [(null? l) id]
      [else (display l)(newline)(compose (eval (car l)) (compose-all (cdr l)))]))

(define (compose-all2 f . gs)
  (cond [(null? gs) f]
      [else (display gs)(newline)
            (compose f (apply compose-all2 gs))]))

(define (deep-map f dl)
  (deep-foldr cons f '() dl))

(define (deep-filter p? dl)
  (deep-foldr (lambda (car-res cdr-res)
                (if (and (atom? car-res) (not (p? car-res)))
                    cdr-res
                    (cons car-res cdr-res)))
                (lambda (x) x)
                '()
                dl))

(define (deep-member? x dl)
  (deep-foldr (lambda (y z) (or y z)) (lambda (el) (= el x)) #f dl))
