(import big-chicken)
(import bad spiffy srfi-1 srfi-13)

(define-resource "/"
  (lambda ()
    (send-response
     status: 'ok
     body: "<h1>Hello World!</h1>"
     headers: '((content-type text/html)))))

(define-resource (irregex '(: bol
                              "/mul/"
                              (submatch-named a numeric)
                              "/"
                              (submatch-named b numeric)
                              eol))
  (lambda (match)
    (send-response
     status: 'ok
     body: (let ((a (string->number (irregex-match-substring match 1)))
                 (b (string->number (irregex-match-substring match 2))))
             (number->string (* a b)))
     headers: '((content-type text/html)))))

(define-resource (lambda (path)
                   (cond ((substring-index "blah" path)
                          => (lambda (idx)
                               (list idx)))
                         (else #f)))
  (lambda (idx)
    (send-response
     status: 'ok
     body: (number->string idx)
     headers: '((content-type text/html)))))

(define-resource "/xxx/foo"
  (lambda ()
    (send-response
     status: 'ok
     body: "foo"
     headers: '((content-type text/html)))))

(define-resource (lambda (path)
                   (and (string-prefix? "/add/" path)
                        (filter-map (lambda (item)
                                      (string->number item))
                                    (string-split path "/"))))
  (lambda numbers
    (send-response
     status: 'ok
     body: (number->string (apply + numbers))
     headers: '((content-type text/html)))))

(define-resource (lambda (path)
                   (and (string-prefix? "/split" path)
                        (string-split path "/")))
  (lambda path-parts
    (send-response
     status: 'ok
     body: (string-intersperse path-parts "#")
     headers: '((content-type text/html)))))

(vhost-map `((".*" . ,(dispatch-resources))))

(bad-start)
