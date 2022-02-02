;;;; todo.scm - TODO management for programmers.

(import (chicken file)
        (chicken format)
        (chicken io)
        (chicken process-context)
        (chicken string))

(define-constant default-filename     "TODO")
(define-constant default-line-number  #t)
(define-constant default-sync-message "Update TODOs")

(: err (string -> void))
(define (err str)
  (fprintf (current-error-port) "todo: ~A~%" str)
  (exit 1))

(: list-insert (list fixnum any --> list))
(define (list-insert lst index elem)
  (define (aux lst i elem)
    (cond ((null? lst) (cons elem '()))
          ((= i index) (cons elem lst))
          (else (cons (car lst) (aux (cdr lst) (+ i 1) elem)))))
  (if (negative? index)
      (cons elem lst)
      (aux lst 0 elem)))

(: read-linum (string fixnum -> string))
(define (read-linum filename linum)
  (when (< linum 1)
    (err (sprintf "read-linum: invalid linum value: ~A" linum)))
  (with-input-from-file filename
    (lambda ()
      (do ((i 1 (+ i 1))
           (line (read-line) (read-line)))
          ((= i linum) line)
        (when (eof-object? line)
          (err (sprintf "read-linum: line does not exist: ~A" linum)))))))

(: write-linum (string fixnum string -> void))
(define (write-linum filename linum str)
  (when (< linum 1)
    (err (sprintf "write-linum: invalid linum value: ~A" linum)))
  (define lines (with-input-from-file filename read-lines))
  (when (> linum (length lines))
    (err (sprintf "write-linum: invalid linum value: ~A" linum)))
  (with-output-to-file filename
    (lambda ()
      (do ((i 1 (+ i 1))
           (lst lines (cdr lst)))
          ((null? lst))
        (write-line (if (= i linum) str (car lst)))))))

(: insert-linum (string fixnum string -> void))
(define (insert-linum filename linum str)
  (when (< linum 1)
    (err (sprintf "insert-linum: invalid linum value: ~A" linum)))
  (define lines (with-input-from-file filename read-lines))
  (when (> linum (+ (length lines) 1))
    (err (sprintf "insert-linum: invalid linum value: ~A" linum)))
  (set! lines (list-insert lines (- linum 1) str))
  (with-output-to-file filename
    (lambda ()
      (do ((lst lines (cdr lst)))
          ((null? lst))
        (write-line (car lst))))))

(: drop-linum (string fixnum -> void))
(define (drop-linum filename linum)
  (when (< linum 1)
    (err (sprintf "drop-linum: invalid linum value: ~A" linum)))
  (define lines (with-input-from-file filename read-lines))
  (when (> linum (length lines))
    (err (sprintf "drop-linum: invalid linum value: ~A" linum)))
  (with-output-to-file filename
    (lambda ()
      (do ((i 1 (+ i 1))
           (lst lines (cdr lst)))
          ((null? lst))
        (unless (= i linum)
          (write-line (car lst)))))))

(: database-exists? (string -> boolean))
(define (database-exists? filename)
  (file-exists? filename))

(: database-create (string -> void))
(define (database-create filename)
  (with-output-to-file filename void))

(: todo-flag (string --> string))
(define (todo-flag todo)
  (substring todo 0 4))

(: todo-replace-flag (string string --> string))
(define (todo-replace-flag todo flag)
  (string-append flag (substring todo 4 (string-length todo))))

;; Add a TODO with the message STR to the end of FILENAME.
(: cmd-add (string string -> void))
(define (cmd-add str filename)
  (with-output-to-file filename
    (lambda () (print "TODO " str))
    #:append))

;; Mark the TODO at line LINUM of FILENAME as ABRT.
(: cmd-abort (fixnum string -> void))
(define (cmd-abort linum filename)
  (define line (read-linum filename linum))
  (write-linum filename linum (todo-replace-flag line "ABRT")))

;; Mark the TODO at line LINUM of FILENAME as DONE.
(: cmd-done (fixnum string -> void))
(define (cmd-done linum filename)
  (define line (read-linum filename linum))
  (write-linum filename linum (todo-replace-flag line "DONE")))

;; Replace the message of the TODO at line LINUM of FILENAME with STR.
(: cmd-edit (fixnum string string -> void))
(define (cmd-edit linum str filename)
  (write-linum filename linum
               (sprintf "~A ~A" (todo-flag (read-linum filename linum)) str)))

;; Insert a new TODO with the message STR at line LINUM of FILENAME.
(: cmd-insert (fixnum string string -> void))
(define (cmd-insert linum str filename)
  (insert-linum filename linum (sprintf "TODO ~A" str)))

;; Print the list of TODOs contained within FILENAME.
(: cmd-list (string -> void))
(define (cmd-list filename)
  (define (print-todos)
    (define (print-todo str linum)
      (when default-line-number
        (let* ((linum-str (number->string linum))
               (linum-len (string-length linum-str)))
          (printf "~A~A "
                  (make-string (max 0 (- 2 linum-len)) #\space)
                  linum-str)))
      (print str))
    (do ((line (read-line) (read-line))
         (linum 1 (+ linum 1)))
        ((eof-object? line))
      (print-todo line linum)))
  (with-input-from-file filename print-todos))

;; Drop the TODO at line LINUM of FILENAME.
(: cmd-remove (fixnum string -> void))
(define (cmd-remove linum filename)
  (drop-linum filename linum))

;; Mark the TODO at line LINUM of FILENAME as TODO.
(: cmd-todo (fixnum string -> void))
(define (cmd-todo linum filename)
  (define line (read-linum filename linum))
  (write-linum filename linum (todo-replace-flag line "TODO")))

(: main (char #!optional fixnum string -> void))
(define (main cmd #!optional (linum 0) (str ""))
  (unless (database-exists? default-filename)
    (database-create default-filename))
  (case cmd
    ((#\a) (cmd-add          str default-filename))
    ((#\b) (cmd-abort  linum     default-filename))
    ((#\d) (cmd-done   linum     default-filename))
    ((#\e) (cmd-edit   linum str default-filename))
    ((#\h) (cmd-help                             )) ; TODO
    ((#\i) (cmd-insert linum str default-filename))
    ((#\l) (cmd-list             default-filename))
    ((#\r) (cmd-remove linum     default-filename))
    ((#\s) (cmd-sync             default-filename)) ; TODO
    ((#\t) (cmd-todo   linum     default-filename))))

(let ((args (command-line-arguments)))
  (case (length args)
    ((0) (main #\l))
    ((1) (let ((cmd (string-ref (car args) 0)))
           (case cmd
             ((#\h #\l #\s) (main (string-ref (car args) 0)))
             (else (err (sprintf "invalid command: ~A" cmd))))))
    (else
     (let ((cmd (string-ref (car args) 0))
           (linum (string->number (cadr args))))
       (case cmd
         ((#\a) (main cmd 0 (string-intersperse (cdr args))))
         ((#\b #\d #\r #\t)
          (if linum
              (main cmd linum)
              (err (sprintf "invalid linum: ~A" (cadr args)))))
         ((#\e #\i)
          (cond ((not linum) (err (sprintf "invalid linum: ~A" (cadr args))))
                ((< (length args) 3) (err "invalid argument count"))
                (else (main cmd linum (string-intersperse (cddr args))))))
         ((#\h #\l #\s) (main cmd))
         (else (err (sprintf "invalid command: ~A" cmd))))))))
