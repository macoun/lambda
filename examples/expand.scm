
(define (translate literals pattern template input)
  (define patvars (pattern-vars pattern literals))
  (define binding (match-pattern pattern input literals))
  (define expanded (expand-template template binding patvars))
  (newline)
  (display "Pattern: ")
  (newline)
  (display pattern)
  (newline)
  (display "Template: ")
  (newline)
  (display template)
  (newline)
  (display "Pattern vars: ")
  (newline)
  (display patvars)
  (newline)
  (display "Input: ")
  (newline)
  (display input)
  (newline)
  (display "Binding: ")
  (newline)
  (display binding)
  (newline)
  (display "Expanded: ")
  (newline)
  (display expanded)
  (newline)
  expanded)

(translate 
  '()
  '(q (a ...) (b ...) ...) 
  '(((a b q) ...) ...) 
  '( 45 (1 2 3) (4 5 6) (7 8 9)))

(translate 
  '()
  '(a t (b c ) ...  (e f) d g o ) 
  '(a (c) ... (d e)) 
  '(1 "hello" (2 3) (4 5) (40 50) (60 70) (6) (7) (8)))

(translate
  '()
  '((((a b) ...) ...) ...)
  '(((_ (a ...) (b ...) ...) ...) ...)
  '((((1 2) (3 4)) ((5 6) (7 8))) (((9 10) (11 12)) ((13 14) (15 16)))))

(define (iter n f)
  (define (iter-loop i a)
    (if (eq? i n)
      a
      (iter-loop (+ i 1 3) (f i))))
  (iter-loop 0 0))
  
(translate
  '(=>)
  '(a => b)
  '(if a b (error "Pattern match failure"))
  '(#t => 42))


; (define a 2)
(display "Iterating... ")
; (iter 10 (lambda (i) (display i) (newline) (translate
;   '(=>)
;   '(a => b)
;   '(if a b (error "Pattern match failure"))
;   '(#t => 42))))
