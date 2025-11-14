#lang racket

; =============================================================================
; Упражнение 6: Двоични дървета и задачи за упражнение
; =============================================================================

; =============================================================================
; ДВОИЧНИ ДЪРВЕТА
; =============================================================================

; Основни функции за работа с двоични дървета
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

; Примерно дърво за тестове
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

; Задача 1: traverse - обхождане ляво-корен-дясно
(define (traverse t)
  (if (empty-tree? t)
      '()
      (append (traverse (left-tree t))
              (list (root-tree t))
              (traverse (right-tree t)))))

; Задача 2: level - елементи на дадено ниво
(define (level k t)
  (cond
    [(empty-tree? t) '()]
    [(= k 0) (list (root-tree t))]
    [else (append (level (- k 1) (left-tree t))
                  (level (- k 1) (right-tree t)))]))

; Задача 3: list->bst - създаване на двоично сортирано дърво от списък
(define (bst-insert x t)
  (if (empty-tree? t)
      (make-leaf x)
      (if (< x (root-tree t))
          (make-tree (root-tree t)
                     (bst-insert x (left-tree t))
                     (right-tree t))
          (make-tree (root-tree t)
                     (left-tree t)
                     (bst-insert x (right-tree t))))))

(define (list->bst lst)
  (if (null? lst)
      empty-tree
      (let ([root (car lst)]
            [rest (cdr lst)])
        (define (insert-all tree remaining)
          (if (null? remaining)
              tree
              (insert-all (bst-insert (car remaining) tree)
                          (cdr remaining))))
        (insert-all (make-leaf root) rest))))

; Задача 4: bst-search - търсене в двоично сортирано дърво
(define (bst-search x t)
  (if (empty-tree? t)
      #f
      (let ([root (root-tree t)])
        (cond
          [(= x root) #t]
          [(< x root) (bst-search x (left-tree t))]
          [else (bst-search x (right-tree t))]))))

; =============================================================================
; ЗАДАЧИ ЗА УПРАЖНЕНИЕ
; =============================================================================

; Задача 1а: diffReverse - разлика между число и неговото обърнато число
(define (num-length n)
  (if (< n 10)
      1
      (+ 1 (num-length (quotient n 10)))))

(define (reverse-num n)
  (define (reverse-helper n acc)
    (if (= n 0)
        acc
        (reverse-helper (quotient n 10)
                        (+ (* acc 10) (remainder n 10)))))
  (reverse-helper n 0))

(define (diffReverse n)
  (- n (reverse-num n)))

; Задача 1б: sortDigits - сортиране на цифри в число в низходящ ред
(define (count-digit d n)
  (if (< n 10)
      (if (= n d) 1 0)
      (+ (if (= (remainder n 10) d) 1 0)
         (count-digit d (quotient n 10)))))

(define (sortDigits n)
  (define (helper power digit sum)
    (if (< digit 0)
        sum
        (let ([count (count-digit digit n)])
          (if (= count 0)
              (helper (/ power 10) (- digit 1) sum)
              (helper (/ power 10)
                      digit
                      (+ sum (* digit power)))))))
  (if (= n 0)
      0
      (let ([max-power (expt 10 (- (num-length n) 1))])
        (helper max-power 9 0))))

; Задача 2: permutable? - проверка за пермутируеми функции
(define (calculate-n x f g n)
  (if (= n 0)
      x
      (f (calculate-n x g f (- n 1)))))

(define (permutable? a b f g)
  (define (check-even start end)
    (if (> start end)
        #t
        (if (not (= (calculate-n start f g start)
                    (calculate-n start g f start)))
            #f
            (check-even (+ start 2) end))))
  (if (odd? a)
      (check-even (+ a 1) b)
      (check-even a b)))

; Задача 3: longest-interval-subset-a - подмножества на най-дългия интервал
(define (interval-length pair)
  (- (cdr pair) (car pair)))

(define (longest-interval il)
  (define (helper curr-max best-pair remaining)
    (if (null? remaining)
        best-pair
        (let ([current (car remaining)])
          (if (> (interval-length current) curr-max)
              (helper (interval-length current)
                      current
                      (cdr remaining))
              (helper curr-max best-pair (cdr remaining))))))
  (if (null? il)
      (cons 0 0)
      (helper -1 (car il) (cdr il))))

(define (subset? pair1 pair2)
  (and (>= (car pair2) (car pair1))
       (<= (cdr pair2) (cdr pair1))))

(define (longest-interval-subset-a il)
  (if (null? il)
      '()
      (let ([longest (longest-interval il)])
        (filter (lambda (x) (subset? longest x)) il))))

; Задача 4: quick-sort - бързо сортиране
(define (quick-sort lst)
  (if (null? lst)
      '()
      (let ([pivot (car lst)]
            [rest (cdr lst)])
        (append (quick-sort (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quick-sort (filter (lambda (x) (>= x pivot)) rest))))))

; =============================================================================
; ТЕСТОВЕ И ПРИМЕРИ
; =============================================================================

(displayln "=== Двоични дървета ===")
(displayln (format "traverse t: ~a" (traverse t)))
(displayln (format "level 1 t: ~a" (level 1 t)))
(displayln (format "level 2 t: ~a" (level 2 t)))

(displayln "\n=== BST операции ===")

(define bst (list->bst '(5 2 8 1 3 7 9)))
(displayln bst)
((list->bst '(10 5 15 3 7)))
(displayln (list->bst '(1)))
(displayln (list->bst '()))

(displayln (format "bst-search 3: ~a" (bst-search 3 bst)))
(displayln (format "bst-search 7: ~a" (bst-search 7 bst)))
(displayln (format "bst-search 10: ~a" (bst-search 10 bst)))
(displayln (format "bst-search 0: ~a" (bst-search 0 bst)))

(displayln "\n=== Задача 1а: diffReverse ===")
(displayln (format "diffReverse 7641: ~a" (diffReverse 7641)))
(displayln (format "diffReverse 123: ~a" (diffReverse 123)))
(displayln (format "diffReverse 7: ~a" (diffReverse 7)))

(displayln "\n=== Задача 1б: sortDigits ===")
(displayln (format "sortDigits 6174: ~a" (sortDigits 6174)))
(displayln (format "sortDigits 9912939: ~a" (sortDigits 9912939)))
(displayln (format "sortDigits 0: ~a" (sortDigits 0)))
(displayln (format "sortDigits 123456789: ~a" (sortDigits 123456789)))

(displayln "\n=== Задача 2: permutable? ===")
(displayln (format "permutable? 1 9 (x*x) (x*x*x): ~a"
                   (permutable? 1 9 (lambda (x) (* x x)) (lambda (x) (* x x x)))))
(displayln (format "permutable? 1 9 (x+1) (x-2): ~a"
                   (permutable? 1 9 (lambda (x) (+ x 1)) (lambda (x) (- x 2)))))
(displayln (format "permutable? 1 9 (x*x) (x/2): ~a"
                   (permutable? 1 9 (lambda (x) (* x x)) (lambda (x) (/ x 2)))))
(displayln (format "permutable? 1 9 (x*x) (x+100): ~a"
                   (permutable? 1 9 (lambda (x) (* x x)) (lambda (x) (+ 100 x)))))

(displayln "\n=== Задача 3: longest-interval-subset-a ===")
(define test '((24 . 25) (90 . 110) (0 . 100) (10 . 109) (1 . 3) (-4 . 2)))
(displayln (format "longest-interval-subset-a test: ~a" (longest-interval-subset-a test)))

(displayln "\n=== Задача 4: quick-sort ===")
(displayln (format "quick-sort '(5 2 8 1 9 3): ~a" (quick-sort '(5 2 8 1 9 3))))
(displayln (format "quick-sort '(10 5 2 7): ~a" (quick-sort '(10 5 2 7))))
(displayln (format "quick-sort '(1): ~a" (quick-sort '(1))))
(displayln (format "quick-sort '(): ~a" (quick-sort '())))
(displayln (format "quick-sort '(5 5 2 5 2): ~a" (quick-sort '(5 5 2 5 2))))
