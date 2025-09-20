; (display 
; (let loop ((x 5) (y 10))
;   (if (eq? x 0)
;       y
;       (loop (- x 1) (+ y 1))))
; )
; ; ==> 15

; (newline)
; (display
;  (let ((x 2) (y 3))
;    (+ x y))
; )
; ; ==> 5

; (define a 10)
; (define (f x) (set! g x) #t)
; (newline)
; (display "Before F= " a)
; (newline)
; (f 5)
; (display "After F= " a)
; ; ==> 5
; (newline)



(assert-equal (quasiquote (0 1 2)) '(0 1 2) "quasiquote simple")
(assert-equal (quasiquote (0 (unquote (+ 1 2)) 4))
              '(0 3 4)
              "quasiquote unquote")
(assert-equal (quasiquote (0 (unquote-splicing (list 1 2)) 4))
              '(0 1 2 4)
              "quasiquote unquote-splicing")
(assert-equal (quasiquote (0 (unquote-splicing 1)))
              '(0 . 1)
              "quasiquote unquote-splicing single")

(define b 5)
(define s (list 1 2))

(assert-equal  `(a ,b c) 
               '(a 5 c)
               "quasiquote unquote")
(assert-equal  `(a ,s c) '(a (1 2) c)
               "quasiquote unquote-splicing")
(assert-equal  `(a ,@s c) '(a 1 2 c)
               "quasiquote unquote-splicing single")