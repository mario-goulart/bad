(module bad

(bad-start
 define-resource
 dispatch-resources
 )

(import scheme)
(import (chicken base)
        (chicken irregex)
        (chicken string))
(import intarweb spiffy srfi-18 srfi-69 uri-common)

(define *bad-routing-table*
  ;; Nested hash tables:
  ;; {
  ;;     <method>: {
  ;;         <type>: {
  ;;             <matcher>: <handler>
  ;;         }
  ;;     }
  ;; }
  (make-hash-table))

(define (find-resource method req-path)
  (and-let* ((method-ht (hash-table-ref/default *bad-routing-table* method #f)))
    (let ((result #f))
      (let loop-types ((types '(string procedure regex))) ;; Order is relevant here
        (if (null? types)
            #f
            (let ((type-ht (hash-table-ref method-ht (car types))))
              (let loop-matchers ((matchers (hash-table-keys type-ht)))
                (if (null? matchers)
                    (loop-types (cdr types))
                    (let* ((matcher (car matchers))
                           (handler (hash-table-ref type-ht matcher)))
                      (case (car types)
                        ((string)
                         (when (string=? req-path matcher)
                           (set! result handler)))
                        ((regex)
                         (and-let* ((match (irregex-search matcher req-path)))
                           (set! result (lambda () (handler match)))))
                        ((procedure)
                         (when (matcher req-path)
                           (set! result (lambda () (handler req-path))))))
                      (or result
                          (loop-matchers (cdr matchers))))))))))))

(define (define-resource matcher handler #!key (method 'GET))
  (let ((matcher-type
         (cond ((string? matcher)    'string)
               ((irregex? matcher)   'regex)
               ((procedure? matcher) 'procedure)
               (else (error "Invalid type:" matcher))))
        (method-ht (hash-table-ref/default *bad-routing-table* method #f)))
    (if method-ht
        (let ((type-ht (hash-table-ref method-ht matcher-type)))
          (hash-table-set! type-ht matcher handler))
        (let ((method-ht (make-hash-table)))
          (hash-table-set! *bad-routing-table* method method-ht)
          (for-each
           (lambda (type)
             (let ((type-ht (make-hash-table)))
               (when (eq? type matcher-type)
                 (hash-table-set! type-ht matcher handler))
               (hash-table-set! method-ht type type-ht)))
           '(string regex procedure))))))

(define (request-path request)
  (string-append "/"
                 (string-intersperse
                  (cdr (uri-path (request-uri request)))
                  "/")))

(define (dispatch-resources)
  (lambda (continue)
    (let* ((request (current-request))
           (resource
            (find-resource (request-method request) (request-path request))))
      (when resource
        (resource)
      (continue)))))

(define (bad-start #!key start-thread?)
  (if start-thread?
      (thread-start! start-server)
      (start-server)))

) ;; end module
