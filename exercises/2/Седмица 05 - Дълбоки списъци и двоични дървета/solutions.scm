(define dl '((1 2) ((3 (4) 5) (((6)))) (7)))

(define (atom? l)
  (and (not (null? l))
       (not (pair? l))))

(define (count-atoms l)
  (cond ((null? l) 0)
        ((atom? l) 1)
        (else (+ (count-atoms (cdr l))
                 (count-atoms (car l))))))

(define (deep-foldr op term nv l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-foldr op term nv (car l))
                  (deep-foldr op term nv (cdr l))))))

(define (deep-map f l)
  (deep-foldr cons f '() l))

(define (deep-filter pred? l)
  (deep-foldr (lambda (v h)
                (if (null? v) h
                    (cons v h)))
              (lambda (x)
                (if (pred? x) x '()))
              '() l))

(define (flatten l)
  (deep-foldr append list '() l))
                    
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (list root '() '()))
(define root car)
(define left cadr)
(define right caddr)
(define empty? null?)

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

(define (height tree)
  (if (empty? tree) 0
      (+ 1 (max (height (left tree))
                (height (right tree))))))

(define (leaf? tree)
  (and (not (empty? tree))
       (empty? (left tree))
       (empty? (right tree))))

(define (count-leafs tree)
  (cond ((empty? tree) 0)
        ((leaf? tree) 1)
        (else (+ (count-leafs (left tree))
                 (count-leafs (right tree))))))

(define (map-tree f tree)
  (if (empty? tree) tree
      (make-tree (f (root tree))
                 (map-tree f (left tree))
                 (map-tree f (right tree)))))

(define (tree-to-list tree)
  (if (empty? tree) '()
      (append (tree-to-list (left tree))
              (list (root tree))
              (tree-to-list (right tree)))))

(define (level n tree)
  (cond ((empty? tree) '())
        ((zero? n) (list (root tree)))
        (else (append (level (- n 1)
                             (left tree))
                      (level (- n 1)
                             (right tree))))))

(define (cons#f a b)
  (and b (cons a b)))

(define (path-to x tree)
  (cond ((empty? tree) #f)
        ((equal? x (root tree)) (list (root tree)))
        (else (cons#f (root tree)
                      (or (path-to x (left tree))
                          (path-to x (right tree)))))))

(define derivation-tree (make-tree +
                                   (make-tree -
                                              (make-tree +
                                                         (make-leaf 5)
                                                         (make-leaf 7))
                                              (make-leaf 3))
                                   (make-tree *
                                              (make-tree +
                                                         (make-tree /
                                                                    (make-leaf 20)
                                                                    (make-leaf 4))
                                                         (make-leaf 9))
                                              (make-leaf 6))))

(define (calculate tree)
  (cond ((empty? tree) #f)
        ((leaf? tree) (root tree))
        (else ((root tree)
               (calculate (left tree))
               (calculate (right tree))))))

               





