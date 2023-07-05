(module (garlic strings)
    (char-whitespace?
     trim-space trim-space-left trim-space-right
     string-fields string-lines
     string-split-pred string-split-pred-all
     string-split string-split-all
     string->stream stream->string)
  (import (garlic base) (srfi-13) (srfi-14))
  (reexport (srfi-13) (srfi-14))

  (define (string-empty? s) (= 0 (string-length s)))
  (define (string->stream s) (-> s (string->list) (list->stream)))
  (define (stream->string s) (-> s (stream->list) (list->string)))
  (define (string-reverse s) (-> s (string->list) (reverse) (list->string)))

  (define (string-split-pred pred s)
    (let* ([s (string->list s)]
           [head (take-while (lambda (c) (not (pred c))) s)]
           [rest (drop s (length head))]
           [tail (if [= 0 (length rest)]
                     #f
                     (list->string (drop rest 1)))])
      (values (list->string head) tail)))

  (define (string-split-pred-all pred s)
    (define (split-iter s ag)
      (receive (first rest) (string-split-pred pred s)
        (let ([ag (append ag (list first))])
          (if rest (split-iter rest ag) ag))))
    (filter-not string-empty? (split-iter s '())))

  (define (string-split sep s) (string-split-pred (lambda (c) (equal? c sep)) s))
  (define (string-split-all sep s) (string-split-pred-all (lambda (c) (equal? c sep)) s))

  (define (trim-space-left s)
    (-> (lambda (c) (char-set-contains? char-set:whitespace c))
        (stream-take-while (string->stream s))
        (stream-length)
        (stream-drop (string->stream s))
        (stream->string)))

  (define (trim-space-right s)
    (-> (string-reverse s)
        (trim-space-left)
        (string-reverse)))

  (define (trim-space s) (-> s (trim-space-left) (trim-space-right)))

  (define (string-fields s) (string-tokenize s))
  (define (string-lines s) (string-split-all #\newline s))
  )
