(module (garlic sys) (sh sh-lines slurp spit)
  (import (garlic base) (garlic http)
          (chicken process) (chicken file) (chicken file posix))
  (reexport (chicken process) (chicken file) (chicken file posix))

  (define (slurp file)
    (if (url? file)
        (resp-body (http-get file))
        (with-input-from-file file read-string)))

  (define (spit file s)
    (let* ([mode '(+ open/wronly open/append open/creat)]
           [fileno (file-open file mode)]
           [port (open-output-file* fileno #:append)])
      (write s port)
      (close-output-port port)))

  ;; easy shell to string
  (define (sh cmd) (with-input-from-pipe cmd read-string))

  ;; stream of lines from shell command
  ;; closes pipe when done reading

  (define (sh-lines cmd)
    (let ([pipe (open-input-pipe cmd)])
      (define (recur)
        (let ([line (read-line pipe)])
          (if (eof-object? line)
              (begin (close-input-pipe pipe) stream-null)
              (stream-cons line (recur)))))
      (recur)))

  ;; bruh

  ;; (define (sh-lines cmd)
  ;;   (->> (cons "" (open-input-pipe cmd))
  ;;        (stream-iterate
  ;;         (lambda (p) (let* ([pipe (cdr p)]
  ;;                       [line (read-line pipe)])
  ;;                  (if (eof-object? line)
  ;;                      (begin (close-input-pipe pipe) stream-null)
  ;;                      (cons line pipe)))))
  ;;        (stream-drop 1)
  ;;        (stream-map car)))

  ;; (define-stream (sh-lines cmd)
  ;;   (stream-let
  ;;    loop ((pipe (open-input-pipe cmd))
  ;;          (line (read-line pipe)))
  ;;    (if (eof-object? line)
  ;;        (begin (close-input-pipe pipe) stream-null)
  ;;        (stream-cons line (loop (read-line pipe) pipe)))))

  ;; (define-stream (slow-stream count)
  ;;   (stream-let loop ((count count))
  ;;               (if (= count 0)
  ;;                   stream-null
  ;;                   (begin (sleep 1) (stream-cons count (loop (sub1 count)))))))

  ;; (define (sh-lines cmd)
  ;;   (let ([pipe (open-input-pipe cmd)]
  ;;         [chan (gochan 0)])
  ;;     (define (recur)
  ;;       (dbg "READING")
  ;;       (let ([line (read-line pipe)])
  ;;         (if (eof-object? line)
  ;;             (begin
  ;;               (dbg "CLOSING")
  ;;               (close-input-pipe pipe)
  ;;               (gochan-close chan))
  ;;             (begin
  ;;               (dbg "SENDING")
  ;;               (gochan-send chan line)
  ;;               (recur)))))
  ;;     (go (recur))
  ;;     chan))
  )
