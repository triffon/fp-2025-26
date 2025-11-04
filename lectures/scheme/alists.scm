(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

(define al '((1 . 2) (2 . 3) (3 . 4)))

(define (assv key al)
  (search (lambda (kv) (if (eqv? (car kv) key) kv #f)) al))