;; Define a when macro
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

; (define-syntax when (syntax-rules () ((when test body ...) (if test (begin body ...)))))
; (when (> 3 2) (display "3 is greater than 2"))
;; Use the macro
(when (> 3 2)
      (display "3 is greater than 2")
      (newline))

; Define an unless macro
(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...)))))

;; Define a more complex let macro
(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; Use the let macro
(let ((x 10) (y 20))
  (display (+ x y))
  (newline))

; ;; Define a simple repeat macro
; (define-syntax repeat
;   (syntax-rules ()
;     ((repeat n body ...)
;      (if (> n 0)
;          (begin body ... (repeat (- n 1) body ...)))))) 
; ; ;; Use the repeat macro
; (repeat 3
;   (display "Hello, World!")
;   (newline))


(define-syntax dis
  (syntax-rules ()
    ((_ q (a ...) (b ...) ...) (display '( ( (a b q) ...) ...) ))))
(define-syntax dis2
  (syntax-rules ()
    ((_q a t (b c ) ...  (e f) d g o ) (display '(a (c )... (d  e)) ))))

(define-syntax dis3
  (syntax-rules ()
    ((_ (((a b) ...) ...) ...) (display '(((_ (a ...) (b ...) ...) ...) ...)))))

(display "Macro expansion results:")
(newline)
(dis 45 (1 2 3) (4 5 6 ) (7 8 9))
(newline)
(dis2  1 "hello" (2 3) (4 5) (40 50) (60 70) (6) (7) (8))
(newline)
(dis3 (((1 2) (3 4)) ((5 6) (7 8))) (((9 10) (11 12)) ((13 14) (15 16))))
(newline)

((lambda () (display "Done with macros examples.") (newline) (display "-------------------") (newline)))
