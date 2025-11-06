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

(define (memv-tree? x t)
  (and (not (empty-tree? t))
       (or (eqv? x (root-tree t)) (memv-tree? x (left-tree t)) (memv-tree? x (right-tree t)))))

(define (memv-tree x t)
  (and (not (empty-tree? t))
       (or (and (eqv? x (root-tree t)) t) (memv-tree x (left-tree t)) (memv-tree x (right-tree t)))))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) (list x))
        (else (cons (root-tree t) (or (path-tree x (left-tree t)) (path-tree x (right-tree t)))))))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) (list x))
        (else (let ((left-path (path-tree x (left-tree t))))
                (if left-path (cons (root-tree t) left-path)
                    (let ((right-path (path-tree x (right-tree t))))
                      (if right-path (cons (root-tree t) right-path) #f)))))))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) (list x))
        (else (cons-maybe (root-tree t) (or (path-tree x (left-tree t)) (path-tree x (right-tree t)))))))

(define (cons-maybe h t)
  (if t (cons h t) t))

(define (cons-maybe h t)
  (and t (cons h t)))

(define (path-tree x t)
  (and (not (empty-tree? t))
       (or (and (eqv? x (root-tree t)) (list x))
           ((maybe cons) (root-tree t)
                         (or (path-tree x (left-tree t))
                             (path-tree x (right-tree t)))))))

(define (maybe op)
  (lambda (h t) (and t (op h t))))

