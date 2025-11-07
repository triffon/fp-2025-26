# Упражнение 6: Мутиращи операции

Днес ще решаваме [задачи за упражнение](./tasks.md). Отдолу имате записки за боравене със странични ефекти (мутиращи операции) и в частност четене от и писане във файлове. Може да са ви полезни най-вече за разработката на проекти.

## 1. Мутиращи операции

На лекции вече сте разглеждали различни видове мутиращи операции. Ето техните сигнатури:

### 1.1 Мутиране на променливи

```scheme
(set! variable value)
```

### 1.2 Мутиране на двойки

```scheme
(set-car! pair value)
(set-cdr! pair value)
```

---

## 2. Работа с файлове

### 2.1 Четене от файл

```scheme
; Отваряне на файл за четене
(open-input-file filename)

; Четене на ред от файл
(read-line [port])

; Четене на S-израз от файл
(read [port])

; Затваряне на входен порт
(close-input-port port)

; Удобна функция за четене на цял файл като низ
(file->string filename)

; Удобна функция за четене на файл като списък от редове
(file->lines filename)
```

### 2.2 Писане във файл

```scheme
; Отваряне на файл за писане
(open-output-file filename)

; Писане на низ във файл
(write value [port])
(display value [port])

; Писане на ред във файл
(writeln value [port])
(displayln value [port])

; Затваряне на изходен порт
(close-output-port port)

; Удобна функция за писане на низ във файл
(display-to-file str filename)

; Удобна функция за писане на списък от редове във файл
(display-lines-to-file lines filename)
```

### 2.3 Примери

**Пример 1: Четене на файл ред по ред**

```scheme
(define (read-file-lines filename)
  (let ([port (open-input-file filename)])
    (define (read-all-lines acc)
      (let ([line (read-line port)])
        (if (eof-object? line)
            (begin
              (close-input-port port)
              (reverse acc))
            (read-all-lines (cons line acc)))))
    (read-all-lines '())))

(read-file-lines "example.txt")
```

**Пример 2: Писане във файл**

```scheme
(define (write-to-file filename content)
  (let ([port (open-output-file filename)])
    (display content port)
    (close-output-port port)))

(write-to-file "output.txt" "Hello, World!")
```

**Пример 3: Използване на удобни функции**

```scheme
; Четене на цял файл
(define content (file->string "input.txt"))

; Писане във файл
(display-to-file "Hello, World!" "output.txt")
```

### Задачи за работа с файлове ✍️

**Задача 1:** Напишете функция `count-lines`, която приема име на файл и връща броя на редовете в него.

```scheme
(count-lines "example.txt")  ; => 5 (ако файлът има 5 реда)
```

**Задача 2:** Напишете функция `reverse-file`, която приема име на входен файл и име на изходен файл, и записва редовете от входния файл в обратен ред във изходния файл.

```scheme
(reverse-file "input.txt" "output.txt")
; Ако input.txt съдържа:
;   Line 1
;   Line 2
;   Line 3
; То output.txt трябва да съдържа:
;   Line 3
;   Line 2
;   Line 1
```
