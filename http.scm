(module (garlic http) (url? fetch-html fetch-json http-req http-get http-post make-resp resp? resp-status resp-headers resp-body resp-status-code)
  (import (garlic base)
          (medea) (html-parser)
          (http-client) (intarweb) (uri-common))

  (define-record-type resp
    (make-resp status headers body) resp?
    (status resp-status)
    (headers resp-headers)
    (body resp-body))

  (define (resp-status-code resp) (cadr (assoc (resp-status resp) (http-status-codes))))

  (define (url? s) (or (string-prefix? "https://" s) (string-prefix? "http://" s)))

  (define (http-req% proc method url body req-headers)
    (receive (body uri resp)
        (with-input-from-request (make-request method: method
                                               uri: (uri-reference url)
                                               headers: (headers req-headers))
                                 body proc)
      (make-resp (response-status resp)
                 (headers->list (response-headers resp))
                 body)))

  (define (http-req method url body headers) (http-req% read-string method url body headers))
  (define (http-get url . headers) (http-req 'GET url #f headers))
  (define (http-post url body . headers) (http-req 'POST url body headers))

  (define (read-json-clean port) (receive (obj) (read-json port) obj))

  (define (fetch-html url . headers) (resp-body (http-req% html->sxml 'GET url #f headers)))
  (define (fetch-json url . headers) (resp-body (http-req% read-json-clean 'GET url #f headers))))
