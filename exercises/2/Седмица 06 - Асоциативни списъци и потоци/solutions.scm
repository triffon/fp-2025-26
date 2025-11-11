(define (foldr op nv l)
  (if (null? l) nv
      (op (car l)
          (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (filter p? l)
  (foldr (lambda (c r)
           (if (p? c)
               (cons c r) r))
         '() l))

(define (make-assoc f l)
  (map (lambda (x)
         (cons x (f x))) l))

(define assoc-list (make-assoc (lambda (x) (* x x)) '(1 2 3 4 5)))

(define (keys al)
  (map car al))

(define (values* al)
  (map cd al))

(define (search key al)
  (foldr (lambda (current result)
           (if (equal? (car current) key)
               (cdr current)
               result))
         #f al))

(define (add-assoc key value al)
  (cons (cons key value)
        (filter (lambda (pair)
                  (not (equal? (car pair) key)))
                al)))

(define (add-assoc* key value al)
  (let ((existing (search key al)))
    (if (not existing)
        (cons (cons key value) al)
        (map (lambda (pair)
               (if (equal? key (car pair))
                   (cons key value)
                   pair)) al))))

(define (histogram lst)
  (foldl (lambda (result current)
           (let ((existing (search current result)))
             (add-assoc* current (+ 1 (or existing 0)) result)))
         '() lst))

(define (group-by f lst)
  (foldr (lambda (current result)
           (let* ((key (f current))
                 (existing (search key result)))
             (add-assoc* key (cons current (or existing '())) result)))
         '() lst))

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s))) 

(define (take n lst)
  (if (or (null? lst)
          (zero? n))
      '()
      (cons (head lst)
            (take (- n 1)
                  (tail lst)))))

(define (from n)
  (cons-stream n (from (+ n 1))))
  
(define nats (from 0))     

(define (map-stream f lst)
  (if (null? lst) '()
      (cons-stream (f (head lst))
                   (map-stream f (tail lst)))))

(define nats*
  (cons-stream 0 (map-stream (lambda (x) (+ 1 x)) nats*)))

(define (fibs-iter a b)
  (cons-stream (+ a b)
               (fibs-iter (+ a b) a)))

(define fibs
  (cons-stream 0 (cons-stream 1 (fibs-iter 1 0))))

(define (zip-with-stream f l1 l2)
  (if (or (null? l1)
          (null? l2))
      '()
      (cons-stream (f (head l1)
                      (head l2))
                   (zip-with-stream f (tail l1) (tail l2)))))

(define fibs*
  (cons-stream
   0
   (cons-stream
    1
    (zip-with-stream + fibs* (tail fibs*)))))

(define (zip-with f l1 l2)
  (if (or (null? l1)
          (null? l2))
      '()
      (cons (f (car l1)
               (car l2))
            (zip-with f (cdr l1) (cdr l2)))))

(define (next-pascal-row row)
  (let ((current (zip-with +
                           (cons 0 row)
                           (append row (list 0)))))
  (cons-stream current
               (next-pascal-row current))))

(define pascal-triangle
  (cons-stream (list 1)
               (next-pascal-row (list 1))))

(define (sum-last k n)
  (define (sum-last-n last)
    (let ((last-sum (apply + last)))
      (cons-stream last-sum
                   (sum-last-n (append (if (= (length last) n)
                                           (cdr last) last)
                                       (list last-sum))))))
  (cons-stream k (sum-last-n (list k))))

(define (average x . xs)
  (/ (apply + x xs)
     (+ 1 (length xs))))





