
(define thread-list '())
(define (make-cont cont dummy) cont)

(define thread-yield
  (lambda ()
    (if (null? thread-list)
      #t
      (let ([cont (make-cont (call/cc (lambda (c) c)) #f)])
        (if (procedure? cont)
          (let* ([next-thread (car thread-list)]
                 [remaining-threads (cdr thread-list)]
                 [new-thread-list (append remaining-threads (list cont))])
            (set! thread-list new-thread-list)
            (displayln "Removed chunk: " (length thread-list) " threads")
            (next-thread 'your-turn))
            #t)))))


(define thread-end
  (lambda ()
    (displayln "Thread ending. Remaining threads: " (length thread-list))
    (if (null? thread-list)
      (begin (displayln "No more threads. Exiting.") (exit 0))
      (let* ([next-thread (car thread-list)]
            [remaining-threads (cdr thread-list)])
        (displayln "Ended chunk: " (length thread-list) " threads"  )
        (set! thread-list remaining-threads)
        (displayln "Calling next-thread: " (length thread-list) " threads"  )
        (next-thread 'your-turn)))))



(define thread-new
  (lambda (thread-proc)
    (let ([cont (make-cont (call/cc (lambda (c) c)) #f)])
      (if (procedure? cont)
        (begin      
          (set! thread-list (append thread-list (list cont)))
          (displayln "Added new chunk: " (length thread-list) " threads")
          (thread-proc)
          (displayln "Thread proc ended, calling thread-end. Remaining threads: " (length thread-list) " threads" )
          (thread-end)
          (displayln "No threads to end: " (length thread-list) " threads" ))
        #t))))

(define t1-proc
  (lambda ()
    (displayln "  ## Hello from 1: " (length thread-list) " threads")
    (thread-yield)
    (displayln "  ## Hello from 1 again: " (length thread-list) " threads")))

(define t2-proc
  (lambda ()
    (displayln "  ## Hello from 2: " (length thread-list) " threads")
    (thread-yield)
    (displayln "  ## Hello from 2 again: " (length thread-list) " threads")
    (thread-yield)
    (displayln "  ## Hello from 2 again again: " (length thread-list) " threads")
    (displayln "********************************************************")))

(define t3-proc
  (lambda ()
    (displayln "  ## Hello from 3: " (length thread-list) " threads")
    (thread-yield)
    (displayln "  ## Hello from 3 again: " (length thread-list) " threads")))

(define t4-proc
  (lambda ()
    (displayln "  ## Hello from 4: " (length thread-list) " threads")
    (thread-yield)
    (displayln "  ## Hello from 4 again: " (length thread-list) " threads")))

(thread-new t1-proc)
(displayln "After adding 1: " (length thread-list) " threads")
(thread-new t2-proc)
(displayln "After adding 2: " (length thread-list) " threads")
(thread-new t3-proc)
(displayln "After adding 3: " (length thread-list) " threads")
; (thread-new t4-proc)
; (displayln "After adding 4: " (length thread-list) " threads")
(displayln "REACHED HERE")
(thread-end)
(displayln "After ending main: " (length thread-list) " threads")

