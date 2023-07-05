(module (garlic base) (begin1
                       swap!
                       enumerate
                       $ != curry curry-last
                       dbg filter-not
                       conj get
                       all? none? in?
                       range)
  ;; base modules
  (import scheme (chicken base) (chicken syntax) (chicken module))
  (reexport scheme (chicken base) (chicken syntax) (chicken module))

  ;; chicken modules
  (import (chicken io)
          (chicken pretty-print)
          (chicken port)
          (chicken string))
  (reexport (chicken io) (chicken pretty-print) (chicken port) (chicken string))

  ;; data structures
  (import (srfi-1)                      ; lists
          (srfi-9)                      ; records
          (srfi-13)                     ; strings
          (srfi-14)                     ; chars
          (srfi-41)                     ; streams
          (srfi-69)                     ; hash-tables
          (alist-lib)                   ; alists
          )

  (reexport (srfi-1) (srfi-9) (srfi-13) (srfi-14) (srfi-41) (srfi-69) (alist-lib))

  ;; syntax modules
  (import (clojurian syntax))
  (reexport (clojurian syntax))

  ;; macros

  ;; like `begin` except returns first expression
  (define-syntax begin1
    (syntax-rules ()
      ((_ form1 forms ...)
       (let ((res form1))
         (begin
           forms ...)
         res))))

  ;; update a variable with a function
  (define-syntax swap!
    (syntax-rules ()
      ((_ var f)
       (set! var (f var)))))

  ;; handy utils
  (define (!= a b) (not (= a b)))
  (define (curry f x) (lambda args (apply f (cons x args))))
  (define (curry-last f x) (lambda args (apply f (append args (list x)))))

  (define (dbg x) (pretty-print x) x)
  (define (filter-not pred coll) (filter (lambda (x) (not (pred x))) coll))

  ;; list functions
  (define (all? f seq) (null? (filter (lambda (x) (not (f x))) seq)))
  (define (none? f seq) (null? (filter f seq)))

  ;; stream functions
  (define (range from to)
    (if (>= from to)
        stream-null
        (stream-cons from (range (add1 from) to))))

  ;; generic functions
  (define (enumerate coll)
    (let* ([counter 0]
           [f (lambda (item)
                (begin1
                 (cons counter item)
                 (swap! counter add1)))])
      (cond [(stream? coll) (stream-map f coll)]
            [(list? coll) (map f coll)])))

  (define (in? item coll)
    (define (list-contains? l)
      (cond [(null? l) #f]
            [(equal? item (car l)) #t]
            [else (list-contains? (cdr l))]))
    (define (stream-contains? s)
      (cond [(stream-null? s) #f]
            [(equal? item (stream-car s)) #t]
            [else (stream-contains? (stream-cdr s))]))
    (cond [(list? coll) (list-contains? coll)]
          [(stream? coll) (stream-contains? coll)])
    )

  (define (conj coll . items)
    (define (combine it ag)
      (cond ((list? coll)
             (cond ((list? it)       (apply conj (cons ag it)))
                   ((hash-table? it) (conj ag (hash-table->alist it)))
                   ((pair? it)       (-> (alist-delete (car it) ag) (alist-set (car it) (cdr it))))))
            ((hash-table? coll)
             (cond ((list? it)       (apply conj (cons ag it)))
                   ((hash-table? it) (hash-table-merge it ag))
                   ((pair? it)       (begin (hash-table-set! ag (car it) (cdr it)) ag))))))
    (fold combine coll items))

  (define ($ coll key . default)
    (let ([d (if (null? default) #f (car default))])
      (cond [(list? coll) (if-let [p (assoc key coll)] (cdr p) d)]
            [(hash-table? coll) (hash-table-ref/default coll key d)]
            (else (error "bad type coll" coll)))))

  ;; false if none
  (define (get key coll)
    (cond [(list? coll) (if-let [p (assoc key coll)] (cdr p) #f)]
          [(hash-table? coll) (hash-table-ref/default coll key #f)]
          [(vector? coll) (if [< key (vector-length coll)] (vector-ref coll key) #f)]
          [(string? coll) (get key (list->vector (string->list coll)))]))
  )
