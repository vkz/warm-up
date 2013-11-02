#lang racket

(require racket/runtime-path)
(require web-server/servlet)


(require "model.rkt"
         file/sha1)

(define-runtime-path home ".")
(displayln (path->string home))

(require web-server/dispatch)
(define-values (dispatch blog-url)
  (dispatch-rules
   [("start") start]))

;; (define (start request)
;;   (let* ((maybe-uri (url->string (request-uri request))))
;;     (displayln (format "Uri: ~a ~n URL: ~a" maybe-uri (lookup-url maybe-uri)))
;;     (redirect-to "http://docs.racket-lang.org/" permanently)))


(define (start request)
  (let* ((maybe-uri (url->string (request-uri request))))
    (displayln (format "Uri: ~a ~n URL: ~a" maybe-uri (lookup-url maybe-uri)))
    (if (lookup-url maybe-uri)
        (redirect-to (lookup-url maybe-uri) permanently)
        (show-landing-page request))))

(define (show-landing-page request #:short-url [short-url "Paste url to be shortened here"])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Bitly")
                  (link ((rel "stylesheet")
                         (href "/style.css"))))
            (body (form ((action
                          ,(embed/url shorten-url))
                         (method "post"))
                        (div (input ((name "url") (value ,short-url))))
                        (div(input ((type "submit")))))))))

  (define (shorten-url request)
    (let* ((bindings (request-bindings request))
           (url (extract-binding/single 'url bindings)))
      (show-landing-page (redirect/get)
                         #:short-url (string-append "localhost:8000/" (make-short url)))))

  (send/suspend/dispatch response-generator))

(define (make-short url)
  (define short-url (substring (sha1 (open-input-string url)) 0 5))
  (add-url (string-append "/" short-url) url)
  short-url)

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #t
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths (list (build-path home "htdocs") home)
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:server-root-path home
               #:servlets-root home
               #:servlet-current-directory home
               )
