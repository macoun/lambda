;; Helper for printing test results
(define (call/cc23 f)
  (letrec ((snap #f)
           (k (lambda (v)
                (machine-restore snap v))))
    (set! snap (machine-snapshot))
    (f k)))
(define (call/cc2 f)
  (let ((snap (machine-snapshot)))
    (displayln "IS vector? " (vector? (car snap)) (length snap))
    (displayln "Captured snapshot... Stack: " (vector-length (car snap)) " Regs: " (vector-length (cdr snap)))
    (f (lambda (v)
    (displayln "Continuation called with value:" v)
    (displayln "Restoring snapshot... Stack count: " (vector-length (car snap)) " Regs count: " (vector-length (cdr snap)))
         (machine-restore snap v)))))


(define (assert-equal expected actual description)
  (if (equal? expected actual)
      (displayln "PASS: " description)
      (displayln "FAIL: " description)))

(let ((result (call/cc (lambda (k) 42)))) ; should never be reached
  (assert-equal 42 result "Zero Test"))

(displayln "assert-equal: " assert-equal)
; ------------------------------
; Test 1: Simple early return
; ------------------------------
(let ((result (call/cc
               (lambda (k)
                 (k 42)
                 100)))) ; should never be reached
  (assert-equal 42 result "Simple call/cc early return"))

;; ------------------------------
;; Test 2: Nested call/cc
;; ------------------------------
(let ((f (lambda ()
           (call/cc
            (lambda (k)
              (+ 1 (k 10)))))))
  (assert-equal 10 (f) "Nested call/cc"))

;; ------------------------------
;; Test 3: call/cc as loop break
;; ------------------------------
(let ((sum 0))
  (call/cc
   (lambda (exit)
     (for-each
      (lambda (x)
        (if (> x 3)
            (exit sum)
            (set! sum (+ sum x))))
      '(1 2 3 4 5))))
  (assert-equal 6 sum "call/cc used as loop break"))

;; ------------------------------
;; Test 4: call/cc multiple invocations
;; ------------------------------
(let ((vals '())
      (first? #t)
      (c #f))
  (displayln "Testing call/cc multiple invocations...")
  (call/cc
   (lambda (k)
    (displayln "Inside call/cc...")
     (set! c k)
     (set! vals (cons 1 vals))))
  (if first?
      (begin
        (displayln "First is true")
        (set! first? #f)
        (set! vals (cons 2 vals)))
      (begin
        (displayln "Restarting continuation...")
        (c 'restart)))
  (set! vals (cons 3 vals))
  ; (displayln vals)
  (assert-equal '(3 2 1) vals "call/cc multiple invocation"))

; ;; ------------------------------
; ;; Test 5: Using call/cc for non-local exit in recursion
; ;; ------------------------------
(let ((result
       (call/cc
        (lambda (exit)
          (define (search lst target)
            (cond
              ((null? lst) #f)
              ((eq? (car lst) target) (exit (car lst)))
              (else (search (cdr lst) target))))
          (search '(1 2 3 4 5) 3)))))
  (assert-equal 3 result "call/cc non-local exit in recursion"))


; (define call/cc call-with-current-continuation)

;; ------------------------------
;; Edge Case 1: Continuation called with no arguments
;; ------------------------------
; (let ((res (call/cc (lambda (k) (k)))))
;   ; (display "result of continuation with no args: " res)
;   (assert-equal '() res "Continuation called with no arguments returns empty list"))

;; ------------------------------
;; Edge Case 2: Continuation called multiple times with different values
;; ------------------------------
(define results '())
(let ((c #f))
  (let ((val (call/cc (lambda (k)
                        (set! c k)
                        'first))))
    (displayln "First call/cc returned: " val)
    (set! results (cons val results)))
    (displayln "Results so far: " results)
  (when (not (member 'second results))
    (displayln "Setting results before second continuation call")
    (set! results (cons 'after-second results))
    (displayln "Invoking continuation with 'second")
    (c 'second))
  (when (not (member 'third results))
    (set! results (cons 'after-third results))
    (c 'third))
  (assert-equal '(third after-third second after-second first) results
                "Continuation invoked multiple times with different values"))

;; ------------------------------
;; Edge Case 3: Continuation captured inside conditional
;; ------------------------------
(define ran-assertions? #f)
(let ((c #f)
      (res #f))
  (if #t
      (set! res (call/cc (lambda (k) (set! c k) 'inside)))
      'never)
  (when (not ran-assertions?)
    (set! ran-assertions? #t)
    (assert-equal 'inside res "Continuation captured in conditional returns normally")
    (assert-equal 'jumped (c 'jumped) "Continuation from conditional works outside")))

;; ------------------------------
;; Edge Case 4: Continuation captured inside let binding
;; ------------------------------
(define ran-let-cont-test? #f)
(let ((c #f))
  (let ((x (call/cc (lambda (k)
                      (set! c k)
                      42))))
    (if (not ran-let-cont-test?)
        (begin
          (assert-equal 42 x "Continuation captured in let returns normally")
          (set! ran-let-cont-test? #t)
          (c 'resumed))
        (assert-equal 'resumed x "Continuation resumes let binding"))))
;; ------------------------------
;; Edge Case 5: Continuation escaping nested recursion
;; ------------------------------
(let ((res (call/cc
            (lambda (exit)
              (define (deep n)
                (if (eq? n 0)
                    (exit 'done)
                    (deep (- n 1))))
              (deep 10000)
              'never)))
      )
  (assert-equal 'done res "Continuation escapes deep recursion safely"))

;; ------------------------------
;; Edge Case 6: Continuation passed around as a first-class value
;; ------------------------------
(define ran-grab-cont-test? #f)
(let ((saved #f)
      (final #f))
  (define (grab)
    (call/cc (lambda (k) (set! saved k) 'captured)))
  (set! final (grab))
  (if (not ran-grab-cont-test?)
      (begin
        (assert-equal 'captured final "Continuation saved as first-class value works inside grab")
        (set! ran-grab-cont-test? #t)
        (assert-equal 'outside (saved 'outside) "Continuation invoked later outside grab"))
      (assert-equal 'outside final "Continuation invoked later outside grab")))
