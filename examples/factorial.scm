;
; Recursive factorial example
;
(define (fact n)
  (if (eq? n 0)
    1
    (* n (fact (- n 1)))))

; Test
(display "Factorial (recursive) of 5 is" (fact 5))
(newline)

;
; Iterative factorial example
;
(define (fact-iter n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))

; Test
(display "Factorial (iterative) of 5 is" (fact 5))
(newline)
