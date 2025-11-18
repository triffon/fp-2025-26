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

(define (map-stream f lst)
  (if (null? lst) '()
      (cons-stream (f (head lst))
                   (map-stream f (tail lst)))))

(define sylvester-sequence
  (cons-stream 2 (map-stream (lambda (x) (+ 1 (- (* x x) x))) sylvester-sequence)))