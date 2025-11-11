(define (fact n)
  (if (= n 0) 1
      (* (fact (- n 1)) n)))

(define the-empty-stream '())
;;(define (cons-stream h t) (cons h (delay t)))
(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

(define s (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define s2 (cons-stream 2 (cons-stream x the-empty-stream)))
;; (2 . #<promise>)

;;(define (my-delay x) (lambda () x))
(define-syntax my-delay
  (syntax-rules () ((my-delay x) (lambda () x))))
(define (my-force p) (p))

(define (enum a b)
  (if (> a b) the-empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (take n s)
  (if (or (empty-stream? s) (= n 0)) '()
      (cons (head s) (take (- n 1) (tail s)))))

(define (find-stream p? s)
  (cond ((empty-stream? s) #f)
        ((p? (head s)) s)
        (else (find-stream p? (tail s)))))

(define bigs (enum 1 (fact 10000)))