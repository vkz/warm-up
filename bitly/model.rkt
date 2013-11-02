#lang racket
(provide (all-defined-out))

(define urldb (make-hash))

(define (lookup-url key)
  (hash-ref urldb key #f))
(define add-url (curry hash-set! urldb))
