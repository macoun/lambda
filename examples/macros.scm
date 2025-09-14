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
    ([unless test body ...]
     (if (not test) (begin body ...)))))

;; Define a more complex let macro
(define-syntax let
  (syntax-rules ()
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; Use the let macro
(let ([x 10] [y 20])
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

; (define-syntax cond
;   (syntax-rules (=> else)

;     ((cond (else else1 else2 ...))
;      ;; The (if #t (begin ...)) wrapper ensures that there may be no
;      ;; internal definitions in the body of the clause.  R5RS mandates
;      ;; this in text (by referring to each subform of the clauses as
;      ;; <expression>) but not in its reference implementation of cond,
;      ;; which just expands to (begin ...) with no (if #t ...) wrapper.
;      (if #t (begin else1 else2 ...)))

;     ((cond (test => receiver) more-clause ...)
;      (let ((t test))
;        (cond/maybe-more t
;                         (receiver t)
;                         more-clause ...)))

;     ((cond (generator guard => receiver) more-clause ...)
;      (call-with-values (lambda () generator)
;        (lambda t
;          (cond/maybe-more (apply guard    t)
;                           (apply receiver t)
;                           more-clause ...))))

;     ((cond (test) more-clause ...)
;      (let ((t test))
;        (cond/maybe-more t t more-clause ...)))

;     ((cond (test body1 body2 ...) more-clause ...)
;      (cond/maybe-more test
;                       (begin body1 body2 ...)
;                       more-clause ...))))

; (define-syntax cond/maybe-more
;   (syntax-rules ()
;     ((cond/maybe-more test consequent)
;      (if test
;          consequent))
;     ((cond/maybe-more test consequent clause ...)
;      (if test
;          consequent
;          (cond clause ...)))))

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

(define-syntax start
  (syntax-rules () 
    ((_ rest ...) ((lambda () rest ...)))))

(start 
  (display "Starting macros examples...") 
  (newline) 
  (display "-------------------") 
  (newline))

(begin)
(let ([sres (start)])
  (display "Result of start with no body: ")
  (display sres)
  (newline)
  (display "-------------------")
  (newline))
;((lambda () (display "Done with macros examples.") (newline) (display "-------------------") (newline)))

(define-syntax cond
  (syntax-rules (else)
    ((_ (else expr ...))
      (begin expr ...))
    ((_ (test expr ...))
      (if test (begin expr ...)))
    ((_ (test expr ...) rest ...)
      (if test
          (begin expr ...)
          (cond rest ...)))))

(cond
  ((> 3 2) (display "3 is greater than 2") (newline))
  ((< 3 2) (display "3 is less than 2") (newline))
  (else (display "Neither condition is true") (newline)))
