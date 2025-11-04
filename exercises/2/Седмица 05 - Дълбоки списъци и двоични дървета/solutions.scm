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








