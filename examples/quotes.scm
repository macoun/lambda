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


(display (let-syntax ((swap!
              (syntax-rules ()
                ((_ a b)
                (let ((tmp a))
                  (set! a b)
                  (set! b tmp))))))

  (let ((x 1) (y 2))
    (swap! x y)
    (list x y))))

(define-syntax m
  (syntax-rules ()
    ((_ x) (+ x 1))))

;; Locally shadow m with a new definition
(newline)
(display 
(let-syntax ((m (syntax-rules ()
               ((_ x) (* x 2)))))
  (m 5))) ; => 10
(newline)
(display (m 5)) ; => 6, original m is unaffected