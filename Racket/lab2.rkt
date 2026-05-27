#lang racket
(require rackunit)

; 3.1
(define (f1 x y)
  (*(* x x)(* y y)))

; 3.2 -> x^y
; recursive
(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

; tail recursive
; (https://beautifulracket.com/explainer/recursion.html)
; - in tail recursive functions we do not need to go up the callstack to get the result
; - the recursion case will directly call the function
; - the base case will return the final result (most of the times the accumulator)
(define (tail-pow x y [acc 1])
  (if (= y 0)
      acc
      (tail-pow x (- y 1) (* x acc))))

; 3.3 -> lungimea unei liste
; recursive
(define (len l)
  (if (eq? l '())
      0
      (+ 1 (len (cdr l)))))

; tail recursive
(define (tail-len l [acc 0])
  (if (null? l)
      acc
      (tail-len (cdr l) (+ acc 1))))

; 3.4 -> numarul de numere dintr-o lista
; recursive
(define (nr_numere l)
  (if (null? l)
      0
      (+ (nr_numere (cdr l))
         (if (number? (car l)) 1 0))))

; tail recursive
(define (tail_nr_numere l [acc 0])
  (if (null? l)
      acc
      (tail_nr_numere (cdr l) (+ acc (if (number? (car l)) 1 0)))))

(check-equal? (tail_nr_numere '(1 a 2 b 3 c)) 3)
(check-equal? (tail_nr_numere '(a b c d e f)) 0)
(check-equal? (tail_nr_numere '(1 2 3 4 5 6)) 6)
(check-equal? (tail_nr_numere '()) 0)
(check-equal? (tail_nr_numere (list 1 (list 2 3 4 5) 6 (list 7 8))) 2)

; 3.5 -> toate elementele sunt la fel
; recursive
(define (same l)
  (if (or (null? l) (null? (cdr l)))
      #t
      (and (eq? (car l) (car (cdr l))) (same (cdr l)))))

(check-equal? (same '(2 2 2 2 2)) #t)
(check-equal? (same '(2 3 2 2 2)) #f)
(check-equal? (same '()) #t)
(check-equal? (same '(1)) #t)
(check-equal? (same '(a a a)) #t)

; tail recursive
(define (tail-same l [acc #t])
  (if (or (null? l) (null? (cdr l)))
      acc
      (tail-same (cdr l) (and acc (eq? (car l) (car (cdr l)))))))

(check-equal? (tail-same '(2 2 2 2 2)) #t)
(check-equal? (tail-same '(2 3 2 2 2)) #f)
(check-equal? (tail-same '()) #t)
(check-equal? (tail-same '(1)) #t)
(check-equal? (tail-same '(a a a)) #t)

; 3.6 -> elem de pe pozitia i
; recursive
(define (get l idx)
  (if (or (null? l) (< idx 0))
      #f
      (if (eq? idx 0)
          (car l)
          (get (cdr l) (- idx 1)))))

(check-equal? (get '() 0) #f)
(check-equal? (get '(1 2 3) 0) 1)
(check-equal? (get (list 1 '(2 3) 4 '(5 6 7) 8 9) 3) '(5 6 7))
(check-equal? (get '(1 2 3) -1) #f)

; * tail recursive
(define (tail-get l idx [acc #f])
  (if (or (null? l) (< idx 0))
      acc
      (tail-get (cdr l) (- idx 1) (if (eq? idx 0)
                                      (car l)
                                      acc))))

(check-equal? (tail-get '() 0) #f)
(check-equal? (tail-get '(1 2 3) 0) 1)
(check-equal? (tail-get (list 1 '(2 3) 4 '(5 6 7) 8 9) 3) '(5 6 7))
(check-equal? (tail-get '(1 2 3) -1) #f)

; Tema

; 4.1 -> concatenarea a doua liste
; recursive

(define (concat l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat (cdr l1) l2))))

(check-equal? (concat '(1 2 3) '(a b c d (1 2))) '(1 2 3 a b c d (1 2)))
(check-equal? (concat '("programarea_functionala") '(e fun)) '("programarea_functionala" e fun))
(check-equal? (concat '() '(a b c)) '(a b c))
(check-equal? (concat '(a b c) '()) '(a b c))

;(check-equal? (tail-concat '(1 2 3) '(a b c d (1 2))) '(1 2 3 a b c d (1 2)))
;(check-equal? (tail-concat '("programarea_functionala") '(e fun)) '("programarea_functionala" e fun))
;(check-equal? (tail-concat '() '(a b c)) '(a b c))
;(check-equal? (tail-concat '(a b c) '()) '(a b c))

; 4.2 -> reverse list

; tail recursive
(define (tail-rev l [acc '()])
  (if (null? l)
      acc
      (tail-rev (cdr l) (cons (car l) acc))))

(check-equal? (tail-rev '()) '())
(check-equal? (tail-rev '(a b (c d) o l)) '(l o (c d) b a))
(check-equal? (tail-rev '(1 2 3 4)) '(4 3 2 1))

; recursive
(define (rev l)
  (if (null? l)
      '()
      (append (rev (cdr l)) (list (car l)))))

(check-equal? (rev '()) '())
(check-equal? (rev '(a b (c d) o l)) '(l o (c d) b a))
(check-equal? (rev '(1 2 3 4)) '(4 3 2 1))

; 4.3 -> elem impare

; first of all: parcurgere in adancime pe lista
(define (list-dfs l)
  (cond
    [(null? l) '()]
    [(pair? (car l)) (append
                      (list-dfs (car l))
                      (list-dfs (cdr l)))]
    [else
     (cons (car l) (list-dfs (cdr l)))]

    ))

(check-equal? (list-dfs '()) '())
(check-equal? (list-dfs '(1 (2 3) 4)) '(1 2 3 4))
(check-equal? (list-dfs '(1 (2 ( 5 6 ( 7 8 ) ) 3) 4)) '(1 2 5 6 7 8 3 4))

; recursive
(define (impare l)
  (cond
    [(null? l) '()]
    [(pair? (car l)) (append (impare (car l)) (impare (cdr l)))]
    [(number? (car l)) (if (odd? (car l))
                       (cons (car l) (impare (cdr l)))
                       (impare (cdr l)))]
    [else
     (impare (cdr l))]
    ))

(check-equal? (impare '()) '())
(check-equal? (impare '(2 4 6 8 10)) '())
(check-equal? (impare '(1 2 3 4 5 6)) '(1 3 5))
(check-equal? (impare '(1 3 5 7 9 11)) '(1 3 5 7 9 11))
(check-equal? (impare '(1 2 (3 4) 5 7 8)) '(1 3 5 7))
(check-equal? (impare '(1 2 (3 4 9) 5 7 8)) '(1 3 9 5 7))
(check-equal? (impare '(a b 1 2 c d 3 4 e)) '(1 3))

; 4.4 -> elemente de pe pozitii impare
(define (odd-index-elements l [idx 0])
  (if (null? l)
      '()
      (cond
        [(odd? idx) (cons (car l) (odd-index-elements (cdr l) (+ idx 1)))]
        [else
         (odd-index-elements (cdr l) (+ idx 1))])))

(check-equal? (odd-index-elements '()) '())
(check-equal? (odd-index-elements '(0 1 2 3 4 5)) '(1 3 5))
(check-equal? (odd-index-elements '(0 (1 2 3) 4 5)) '((1 2 3) 5))