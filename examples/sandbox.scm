;
; This file contains nonsense expressions.
; I use it for dirty testing while the code evolves.
; Just ignore it. Or take a look. I don't really care.
;


;-----------------------------------
; Define function with lambda
(define add
  (lambda (x y) (+ x y)))

; Define function with syntactic sugar
(define (mul x y) (* x y))

(begin
  (display "Add:" (add 45 67))
  (newline)
  (display "Mul:" (mul (add 1 45) 67))
  (newline))

;-----------------------------------
; Scope testing. See (f d)
(define d 4)

(define (f x)
  (define d 8)
  (define (g y) (* y d))
  (g (+ x d)))

(display 'F= (f d))
(newline)


;-----------------------------------
; Recursive reduce
(define (sum l)
  (if (null? l) 0
    (begin
      (display "l is " l)
      (newline)
      (+
        (car l)
        (sum (cdr l))))))

(define my-list (quote (1 2 3 4 5 6 7)))
(define s (sum my-list))

(display "Sum is" s)
(newline)

;-----------------------------------
; Recursive loop
(define (iter v i)
  (if [< i (vector-length v)]
    (begin
      (display i (vector-ref v i))
      (newline)
      (iter v (+ i 1)))))

(define (run-vector-test)
  (begin
    (define v (make-vector 'A 'B 1 "hello" "world"))
    (display 'Length= [vector-length v])
    (newline)
    (iter v 0)))

(run-vector-test)

;-----------------------------------
; Iterative loop
(define (iter n f)
  (define (iter-loop i a)
    (if (eq? i n)
      a
      (iter-loop (+ i 1) (+ (* 2 (f i))))))
  (iter-loop 0 0))

(display "Iterating... ")
;(define result (iter 500000 (lambda (i) (+ 1 i))))
(display (make-vector 1 2 3 4 5 6 7 8 9 0))
(define result (iter 500000 (lambda (i) (+ i (vector-length (make-vector 1 2 3 4 5 6 7 8 9 0))))))


(display result)
(newline)
(display "Iterating finished " result)
(newline)

;-----------------------------------
; Test Unicode
(define 😎 "Cool")
(display 😎)
(newline)
(display "I ❤️  Scheme")
(newline)


(display "Iterating... ")
(define result (iter 50000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(display result)
(newline)
(define result (iter 50000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1 2 3))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1))))))
(define result (iter 5000 (lambda (i) (+ i (vector-length (make-vector 1))))))

(iter 3 (lambda (i) (+ i (iter (- 3000  i) (lambda (i) (+ 3 (vector-length (make-vector 1 2 3 4 5 6 7 8 9 0))))))))

(display result)
(newline)