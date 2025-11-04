(define (tree? t)
  (or (null? t)
      (and (list? t) (= (length t) 3) (tree? (cadr t)) (tree? (caddr t)))))

(define empty-tree '())
(define (make-tree x l r) (list x l r))
;; (define make-tree list)
(define (leaf x) (make-tree x empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define t (make-tree 1 (leaf 2) (make-tree 3 (leaf 4) (leaf 5))))

(define (depth t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth (left-tree t)) (depth (right-tree t))))))