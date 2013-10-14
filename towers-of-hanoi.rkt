#lang racket
;; Towers of Hanoi based on Jay McCarthy's post
;; http://jeapostrophe.github.io/2013-06-03-hanoi-post.html

(require racket/stream
         racket/list
         racket/match
         2htdp/image
         2htdp/universe)

(struct move (from to) #:transparent)


;; using "laziness" with Racket streams
;; (stream-cons ..) is a trick to preserve laziness
;; see Jay's post for explanation
(define (towers n from to extra)
  (if (zero? n)
      empty-stream
      (stream-append
       (towers (sub1 n) from extra to)
       (stream-cons
	(move from to) (towers (sub1 n) extra to from)))))

;; visualization
(define disc-width 10)
(define disc-height 10)
(define (draw-disc d)
  (rectangle (* d disc-width) disc-height 'solid "black"))
 
(define (draw-stack s)
  (apply above
         empty-image empty-image
         (map draw-disc s)))
 
(define (draw-stacks n ss)
  (apply
   beside/align
   'top
   (map
    (λ (s)
      (define m (- n (length s)))
      (above (rectangle (* n disc-width)
                        (* m disc-height)
                        'solid "white")
             (draw-stack s)))
    ss)))

;; move
;; !!! I FIND THIS BIT CONFUSING !!!
(define (list-move ss from to)
  (define from-disc (first (list-ref ss from)))
  (for/list ([s (in-list ss)]
             [i (in-naturals)])
    (cond
      [(= i from)
       (rest s)]
      [(= i to)
       (list* from-disc s)]
      [else
       s])))

;; browse
(define (move-disc/forward w)
  (match-define (world ss fs bs) w) ;initially fs holds the solution to Towers
  (cond
    [(stream-empty? fs)
     w]
    [else
     (match-define (and m (move from to)) (stream-first fs))
     (world (list-move ss from to) (stream-rest fs) (cons m bs))]))
 
(define (move-disc/backward w)
  (match-define (world ss fs bs) w)
  (cond
    [(empty? bs)
     w]
    [else
     (match-define (and m (move to from)) (first bs))
     (world (list-move ss from to) (stream-cons m fs) (rest bs))]))

;; show
(struct world (stacks forward backward))
 
(define (show n)
  (define ms (towers n 0 2 1))
  (define s
    (list (build-list n add1)
          empty
          empty))
  (define (draw-world w)
    (draw-stacks n (world-stacks w)))
  (define (move-disc w k)
    (cond
      [(equal? k "left")
       (move-disc/backward w)]
      [(equal? k "right")
       (move-disc/forward w)]
      [else
       w]))
  (big-bang (world s ms empty)
            (on-key move-disc)
            (to-draw draw-world)))

(module+ main
  (command-line
   #:args ([n "5"])
   (show (string->number n))))
