(define (at lst n)
  (cond ((null? lst) #f)
        ((zero? n) (car lst))
        (else (at (cdr lst)
                  (- n 1)))))

(define (take lst n)
  (if (or (null? lst)
          (zero? n))
      '()
      (cons (car lst)
            (take (cdr lst)
                  (- n 1)))))

(define (find lst pred?)
  (cond ((null? lst) #f)
        ((pred? (car lst))
         (car lst))
        (else (find (cdr lst) pred?))))

(define (zip lst1 lst2)
  (if (or (null? lst1)
          (null? lst2))
      '()
      (cons (cons (car lst1)
                  (car lst2))
            (zip (cdr lst1)
                 (cdr lst2)))))

(define (max-repeated lst)
  (define (most-repeated lst prev max-count current)
    (cond ((null? lst) max-count)
          ((equal? prev (car lst))
           (most-repeated (cdr lst)
                          prev
                          (max max-count (+ current 1))
                          (+ current 1)))
          (else (most-repeated (cdr lst)
                               (car lst)
                               max-count
                               1))))
  (if (null? lst) 0
      (most-repeated (cdr lst)
                     (car lst)
                     1 1)))

(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))

(define (map* f lst)
  (foldr (lambda (current result)
           (cons (f current) result))
         '() lst))

(define (unique lst)
  (foldr (lambda (current result)
           (if (member current result)
               result
               (cons current result)))
         '() lst))

(define (unique* lst)
  (foldl (lambda (result current)
           (if (member current result)
               result
               (cons current result)))
         '() lst))

(define (unique** lst)
  (define (helper lst seen)
    (cond ((null? lst) '())
          ((member (car lst) seen)
           (helper (cdr lst) seen))
          (else (cons (car lst)
                      (helper (cdr lst)
                              (cons (car lst) seen))))))
  (helper lst '()))

(define (filter pred? lst)
  (foldr (lambda (current result)
           (if (pred? current)
               (cons current result)
               result)) '() lst))

(define (quick-sort lst)
  (if (null? lst) lst
      (let* ((pivot (car lst))
            (less (filter (lambda (x) (< x pivot))
                          (cdr lst)))
            (greater (filter (lambda (x) (>= x pivot))
                             (cdr lst))))
        (append (quick-sort less)
                (list pivot)
                (quick-sort greater)))))

(define (max-list lst)
  (apply max lst))

(define (any? pred? lst)
  (foldr (lambda (x y)
           (or (pred? x) y)) #f lst))

(define (map-var f . ls)
  (if (any? null? ls)
      '()
      (cons (apply f (map car ls))
            (apply map-var f (map cdr ls)))))

