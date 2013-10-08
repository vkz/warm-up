#lang racket
;; based on Jay McCarthy's post
;; http://jeapostrophe.github.io/2013-04-01-dfs-in-r-post.html
;;
;; TODO:
;; adapt this code to use the data/heap library to turn it
;; into a best-first search or a depth-first search

(require racket/list
         rackunit
         racket/function
         racket/match
         data/queue)

(define node-data car)
(define node-children cdr)
(define example-tree
  '(0
    (1
     (3
      (7)
      (8))
     (4
      (9)
      (10)))
    (2
     (5
      (11)
      (12))
     (6
      (13)
      (14)))))

(define-syntax-rule (while cond body ...)
  (let loop ()
    (when cond
      body ...
      (loop))))

;; imperative approach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "You have a queue, which you add the root to. You keep taking
;; things off the queue and if you find the node you are looking for,
;; you return it. Otherwise, you add its children to the end of the
;; queue and continue. If the queue is ever empty when you try to
;; remove from it, return an indication of failure."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bfs ? root)
  (let/ec return
    (define q (list root))
    (while (not (empty? q))
	(define cur (first q))
	(set! q (rest q))
	(cond
	 [(? (node-data cur))
	  (return cur)]
	 [else
	  (set! q (append q (node-children cur)))]))
    #f))
(check-false (bfs (curry = 15) example-tree))
(check-equal? (bfs (curry = 11) example-tree) '(11))

;; first attempt at functional
;; O(n) in time and space (append is a bummer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fbfs ? root)
  (define (bfs/q q)
    (cond
     [(empty? q) #f]
     [else
      (match-define (cons cur nextq) q)
      (cond
       [(? (node-data cur)) cur]
       [else (bfs/q (append nextq (node-children cur)))])]))
  (bfs/q (list root)))
(check-false (fbfs (curry = 15) example-tree))
(check-equal? (fbfs (curry = 11) example-tree) '(11))

;; second attempt at functional
;; notice that queue is local state not available to clients of bfs
;; therefore we can go imperative inside bfs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ffbfs ? root)
  (define q (make-queue))
  (define (bfs)
    (cond
     [(queue-empty? q) #f]
     [else
      (define cur (dequeue! q))
      (cond
       [(? (node-data cur)) cur]
       [else
	(for-each (curry enqueue! q)
		  (node-children cur))
	(bfs)])]))
  (enqueue! q root)
  (bfs))
(check-false (fbfs (curry = 15) example-tree))
(check-equal? (fbfs (curry = 11) example-tree) '(11))

;; a more Rackety way of writing this
;; nice thing about it is that it's parametrized over the notion of node
;; algorithm knows only uses node's interface, assumes nix about structure
(define (fffbfs ? root)
  (define q (make-queue))
  (for/or ([cur (in-queue q)])
    (cond
     [(? (node-data cur)) cur]
     [else
      (for-each (curry enqueue! q)
		(node-children cur))
      #f])))
(check-false (fbfs (curry = 15) example-tree))
(check-equal? (fbfs (curry = 11) example-tree) '(11))
