(define l '(1 2 3 4 5))

(define (1+ x) (+ x 1))

(define (length l)
  (if (null? l) 0 (1+ (length (cdr l)))))

(define (append l1 l2)
  (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))

(define (reverse l)
  (if (null? l) l (append (reverse (cdr l)) (list (car l)))))

(define (list-tail l n)
  (if (= n 0) l
      (cdr (list-tail l (- n 1)))))

(define (list-ref l n)
  (car (list-tail l n)))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (member x (cdr l)))))
