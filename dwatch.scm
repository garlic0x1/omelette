(import (garlic base))

(define (worker cmd)
  (let ([port (open-output-pipe "")])
    (lambda []
      (close-pipe port)
      (set! port (open-output-pipe cmd)))))

(define (dir-item? it)
  (and (list? it)
       (vector? (cadr it))))

(define (file-modified dir-item)
  (let* ([name (car dir-item)]
         [data (cadr dir-item)]
	     [modified (stat:mtime data)])
    (cons name modified)))

(define (right-type? types dir-item)
  (if (null? types) #f
      (or (string-suffix? (car types) (car dir-item))
	  (right-type? (cdr types) dir-item))))

(define (watch-dir restart file-types modified-alist)
  (let* ([items (file-system-tree ".")]
         [all-files (filter dir-item? items)]
	     [files (filter (lambda (x) (right-type? file-types x)) all-files)]
	     [new-alist (map file-modified files)])
    (when (not (equal? modified-alist new-alist))
      (restart))
    (sleep 1)
    (watch-dir restart file-types new-alist)))

(define (main . args)
  (let ([config (call-with-input-file ".dwatch" read)])
    (watch-dir (worker (cdr (assoc 'cmd config)))
	       (cdr (assoc 'file-types config))
	       '())))

(main)
