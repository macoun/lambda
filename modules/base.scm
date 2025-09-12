;; Define a when macro
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

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
