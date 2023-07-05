(module (garlic html)
    (query-html element-children element-tree-walk element-attrs element-text element-text-recur)
  (import (garlic base) (html-parser))
  (reexport (html-parser))

  (define (html-element? obj)
    (and (not (null? obj))
         (list? obj)
         (symbol? (car obj))
         (not (equal? '@ (car obj)))))

  (define (attr-match? pattern-attr el-attrs)
    (if (equal? 'class (car pattern-attr))
        (string-contains (or (get 'class el-attrs) "") (cdr pattern-attr))
        (equal? (or (get (car pattern-attr) el-attrs) "") (cdr pattern-attr))))

  (define (attrs-match? pattern-attrs el-attrs)
    (if (not pattern-attrs) #t
        (all? (lambda [x] (attr-match? x el-attrs)) pattern-attrs)))

  (define (element-match? pattern el)
    (and (html-element? el)
         (or (equal? (car pattern) '*) (equal? (car pattern) (car el)))
         (if (null? (cdr pattern)) #t
             (attrs-match? (element-attrs pattern) (element-attrs el)))))

  (define (element-attrs el) (if-let [attrs (get '@ (cdr el))] attrs '()))
  (define (element-text el) (->> (filter string? el) (apply string-append)))
  (define (element-children el) (filter html-element? el))

  (define (element-tree-walk el)
    (let ([children (element-children el)])
      (cons el (apply append (map element-tree-walk children)))))

  (define (element-text-recur el)
    (->> (element-tree-walk el)
         (map (lambda [x] (element-text x)))
         (apply string-append)))

  (define (query-html sel doc)
    (filter (lambda [el] (element-match? sel el)) (element-tree-walk doc)))
  )
