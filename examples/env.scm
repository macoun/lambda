((lambda ()
(define (f x y) (+ x y))
(f 2 3)
(display x " " y)
(newline)

(define (frame-vars frame)
  (map car frame))

(define (env-for-each-frame env f)
  (when (not (null? env))
      (f (car env))
      (env-for-each-frame (cdr env) f)))

(define (print-frame frame)
  (displayln (frame-vars frame)))

(define (print-frame-stack)
  (let ([e (env)])
    (define inside #t)
    (env-for-each-frame (env) print-frame)
    (env-for-each-frame e print-frame)))

(print-frame-stack)
(define dummy 'dummy)
(print-frame-stack)

))