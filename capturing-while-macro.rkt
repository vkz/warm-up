#lang racket
(provide while while1 while2)

;; CL defmacro style
(define-syntax (while stx)
  (define subs (syntax->list stx))
  (define it (datum->syntax stx 'it)) ;'it is in the context of the macro-call
  (datum->syntax
   (quote-syntax here)                ;syntax in the context of macro-definition
   `(let loop ()
      (let ([,it ,(cadr subs)])
        (when ,it
          ,@(cddr subs)
          (loop))))))

;; Rackety way with pattern-variables with-syntax used to avoid
;; nesting syntax-case for pattern-variable generation
(define-syntax (while1 stx)
  (syntax-case stx ()
    [(_ test body ...)
     (with-syntax ([it (datum->syntax stx 'it)]) ; introduce a new pattern-variable
       (syntax (let loop ()                      ; default to context of macro-definition
                 (let ([it test])
                   (when it
                     body ...
                     (loop))))))]))

;; Rackety way with shorthands for (syntax ...) and (with-syntax ...)
(define-syntax (while2 stx)
  (syntax-case stx ()
    [(_ test body ...)
     (let ([it (datum->syntax stx 'it)])
       #`(let loop ()                            ; shorthand for (syntax ...)
           (let ([#,it test])                    ; shorthand for wrapping it in (with-syntax ...)
             (when #,it
               body ...
               (loop)))))]))

;; while, while1 and while2 should have the same output
;; notice that 'it definition comes from the macro-transformation
(define x 3)
(set! x 3)

(displayln "Testing while")
(while1 (> x 0)
        (displayln (format "x is ~a -> so  it is ~a" x it))
        (set! x (sub1 x)))

(displayln "Testing while1")
(set! x 3)
(while2 (> x 0)
        (displayln (format "x is ~a -> so  it is ~a" x it))
        (set! x (sub1 x)))

(displayln "Testing while2")
(set! x 3)
(while (> x 0)
  (displayln (format "x is ~a -> so  it is ~a" x it))
  (set! x (sub1 x)))

;; Expected output
;; Testing while
;; x is 3 -> so  it is #t
;; x is 2 -> so  it is #t
;; x is 1 -> so  it is #t
;; Testing while1
;; x is 3 -> so  it is #t
;; x is 2 -> so  it is #t
;; x is 1 -> so  it is #t
;; Testing while2
;; x is 3 -> so  it is #t
;; x is 2 -> so  it is #t
;; x is 1 -> so  it is #t
