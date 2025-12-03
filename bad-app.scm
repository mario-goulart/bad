(import big-chicken)
(import bad spiffy srfi-13)

(define-resource "/"
  (lambda ()
    (send-response
     status: 'ok
     body: "<h1>Hello World!</h1>"
     headers: '((content-type text/html)))))

(define-resource (irregex "^/foo/.*$")
  (lambda (match)
    (send-response
     status: 'ok
     body: (string-append "regex: "
                          (string-translate* (->string match)
                                             '(("<" . "&lt;") (">" . "&gt;"))))
     headers: '((content-type text/html)))))

(define-resource (lambda (path)
                   (substring-index "blah" path))
  (lambda (path)
    (send-response
     status: 'ok
     body: (string-append "path: " path)
     headers: '((content-type text/html)))))

(define-resource "/xxx/foo"
  (lambda ()
    (send-response
     status: 'ok
     body: "foo"
     headers: '((content-type text/html)))))

(vhost-map `((".*" . ,(dispatch-resources))))

(bad-start)
