(define l '(1 2 3 4 5))

(define (1+ x) (+ x 1))


(define (length l)
  (if (null? l) 0
      (1+ (length (cdr l)))))

(define (length l)
  (foldr (lambda (x r) (1+ r)) 0 l))

 
(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (if (null? l) '()
      (append (reverse (cdr l)) (list (car l)))))

(define (snoc x l)
  (append l (list x)))

(define (reverse l)
  (foldr (lambda (x r) (append r (list x))) '() l))

(define (reverse l)
  (foldr snoc '() l))

(define (rcons r x)
  (cons x r))

(define (reverse2 l)
  (foldl (lambda (r x) (cons x r)) '() l))

(define (reverse2 l)
  (foldl rcons '() l))

(define (list-tail l n)
  (if (= n 0) l
      (cdr (list-tail l (- n 1)))))

(define (list-ref l n)
  (car (list-tail l n)))

(define (member? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (member? x (cdr l)))))

(define (member? x l)
  (and (not (null? l)) (or (equal? x (car l)) (member? x (cdr l)))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (else (member x (cdr l)))))

(define (member? x l)
  (foldr  (lambda (y r) (if (equal? y x) #t r)) #f l))

(define (member x l)
  (and (not (null? l)) (or (and (equal? x (car l)) l) (member x (cdr l)))))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (foldr op nv l)
  (if (null? l) nv
      (op   (car l) (foldr op nv (cdr l)))))

(define (map f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))

(define (map f l)
  (foldr (lambda (x r) (cons (f x) r)) '() l))

(define (foldr op nv l)
  (if (null? l) nv
      (op   (car l) (foldr op nv (cdr l)))))

(define (filter p? l)
  (if (null? l) '()
      (if (p? (car l)) (cons (car l) (filter p? (cdr l))) (filter p? (cdr l)))))

(define (filter p? l)
  (foldr (lambda (x r) (if (p? x) (cons x r) r)) '() l))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (maximum l)
  (foldr max (car l) l))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l) 
      (op (car l) (foldr1 op (cdr l)))))

(define (maximum l)
  (foldr1 max l))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (evali l) (eval l (interaction-environment)))

(define (do-define-double s v) (evali (list 'define s (list '+ v v))))