;; ---------------------------
;; Derived syntax
;; ---------------------------

(define-syntax begin
  (syntax-rules () 
    ((_ rest ...) ((lambda () rest ...)))))

(define-syntax when
  (syntax-rules ()
    ((_ test expr ...)
      (if test (begin expr ...) #f))))

(define-syntax unless
  (syntax-rules ()
    ((_ test expr ...)
      (if (not test) (begin expr ...) #f))))

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

(define-syntax let
  (syntax-rules ()
   ;; Named let
    ((_ name ((var init) ...) body1 body2 ...)
     ((letrec ((name (lambda (var ...) body1 body2 ...)))
        name)
      init ...))

    ;; Ordinary let
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

(define-syntax let*
  (syntax-rules ()
    ((_ () body1 body2 ...)
      (begin body1 body2 ...))
    ((_ ((name val) rest ...) body1 body2 ...)
      (let ((name val))
        (let* (rest ...) body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    [(_ ((var init) ...) body1 body2 ...)
     (let ((var #f) ...)
       (set! var init) ...
       body1 body2 ...)]))

(define-syntax letrec*
  (syntax-rules ()
    ;; empty case
    [(_ () body1 body2 ...) 
     (begin body1 body2 ...)]
    
    ;; recursive case
    [(_ ((var init) rest ...) body1 body2 ...)
     (let ((var init))
       (letrec* (rest ...) body1 body2 ...))]))

(define-syntax and
  (syntax-rules ()
    ((_ ) #t)
    ((_ test) test)
    ((_ test rest ...)
      (if test (and rest ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_ ) #f)
    ((_ test) test)
    ((_ test rest ...)
      (let ((x test))
        (if x x (or rest ...))))))

;; ---------------------------
;; List utilities
;; ---------------------------

(define (list . xs) xs)

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (reverse lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst)
        acc
        (loop (cdr lst) (cons (car lst) acc)))))

;; list-ref: 0-based indexing
(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

;; membership predicates
(define (memq x lst)
  (cond
    ((null? lst) #f)
    ((eq? x (car lst)) lst)
    (else (memq x (cdr lst)))))

(define (memv x lst)
  (cond
    ((null? lst) #f)
    ((eqv? x (car lst)) lst)
    (else (memv x (cdr lst)))))

(define (member x lst)
  (cond
    ((null? lst) #f)
    ((equal? x (car lst)) lst)
    (else (member x (cdr lst)))))

;; association lists
(define (assq key alist)
  (cond
    ((null? alist) #f)
    ((eq? key (caar alist)) (car alist))
    (else (assq key (cdr alist)))))

(define (assv key alist)
  (cond
    ((null? alist) #f)
    ((eqv? key (caar alist)) (car alist))
    (else (assv key (cdr alist)))))

(define (assoc key alist)
  (cond
    ((null? alist) #f)
    ((equal? key (caar alist)) (car alist))
    (else (assoc key (cdr alist)))))

;; functional helpers
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (for-each f lst)
  (if (null? lst)
      (values) ;; return zero values
      (begin (f (car lst))
              (for-each f (cdr lst)))))

;; ---------------------------
;; Boolean helpers
;; ---------------------------

(define (not x)
  (if x #f #t))

;; ---------------------------
;; Numeric functions
;; ---------------------------

;; Predicates
(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

;; Parity
(define (even? x) (= (mod x 2) 0))
(define (odd? x) (not (even? x)))

;; Absolute value
(define (abs x)
  (if (negative? x) (- x) x))

;; Comparison extensions
(define (<= x y) (or (< x y) (= x y)))
(define (>= x y) (or (> x y) (= x y)))

(define (min x y) (if (< x y) x y))
(define (max x y) (if (> x y) x y))
