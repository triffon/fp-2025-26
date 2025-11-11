#lang racket

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define t (make-tree 5
                     (make-tree 1
                                (make-tree 4
                                           '()
                                           (make-leaf 13))
                                (make-leaf 3))
                     (make-tree 8
                                (make-tree 0
                                           (make-leaf 10)
                                           (make-leaf 9))
                                (make-leaf 11))))

(define (tree-member? x t)
  (if (empty-tree? t) #f
      (or (equal? (root-tree t) x) (tree-member? x (left-tree t)) (tree-member? x (right-tree t)))))
  

(define (height-tree t)
  (if (empty-tree? t) 0
      (+ 1 (max (height-tree (left-tree t)) (height-tree (right-tree t))))))

(define (is-leaf? t) (and (list t)
                            (= (length t) 3)
                            (empty-tree? (left-tree t))
                            (empty-tree? (right-tree t))))

(define (count-leafs t)
  (cond [(empty-tree? t) 0]
        [(is-leaf? t) 1]
        [else (+ (count-leafs (left-tree t)) (count-leafs (right-tree t)))]))

(define (dfs t)
  (if (empty-tree? t) '()
      (append (dfs (left-tree t)) (list (root-tree t)) (dfs (right-tree t)))))

(define (level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (level (- k 1) (left-tree t)) (level (- k 1) (right-tree t)))]))

(define (make-assoc f l)
  (map (lambda (x)
         (cons x (f x))) l))

(define assoc-list (make-assoc (lambda (x) (* x x)) '(1 2 3 4 5)))

(define (keys assoc-l)
  (map car assoc-l))

(define (values assoc-l)
  (map cdr assoc-l))

(define (search k assoc-l)
  (if (null? assoc-l) #f
      (if (= (caar assoc-l) k) (car assoc-l) (search k (cdr assoc-l)))))

(define (delete k al)
  (filter (lambda (p) (not (equal? (car p) k))) al))

(define (add-assoc k v al)
  (define (helper k v al)
    (if (null? al) '()
        (if (= (caar al) k)
            (cons (cons k v) (helper k v (cdr al)))
            (cons (car al) (helper k v (cdr al))))))
  (if (not (search k al))
      (append al (list (cons k v)))
      (helper k v al)))

(define (member? x l)
  (foldr (lambda (el res) (or (equal? el x) res)) '#f l))

(define (unique l)
  (foldr (lambda (el res) (if (member? el res) res (cons el res))) '() l))

(define (times x l)
  (foldr (lambda (el res) (+ res (if (equal? x el) 1 0))) 0 l))

(define (histogram l)
  (make-assoc (lambda (x) (times x l)) (unique l)))

(define (histogram2 l)
  (foldr (lambda (el res)
          (let ([pair (search el res)])
           (if pair (add-assoc (cons el (+ 1 (cdr pair))) res)
                    (add-assoc (cons el 1) res)
           ))) '() l))

 
