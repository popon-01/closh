(in-package :closh)

(defvar *global-enviroment*)

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output)
  (read-line *standard-input*))

(defun closh-read (str)
  (parse-with-lexer (closh-lexer str) closh-parser))

(defun closh-print (obj &optional (stream t))
  (format stream "~a~%" (dump-to-str obj)))

(defun read-eval-print-loop ()
  (let* ((line (raw-input "USER> "))
         (res (closh-eval (closh-read line)
                          *global-enviroment*)))
    (unless (eq (type-of res) 'exit-signal)
      (closh-print res)
      (read-eval-print-loop))))

(defmacro init-global (&body key-and-values)
  `(progn
     (setf *global-enviroment* (make-instance 'closh-global))
     ,@(iterate (for l on key-and-values by #'cddr)
                (collect
                    `(add-env ,(intern (symbol-name (first l))
                                       :keyword)
                              ,(second l)
                              *global-enviroment*)))))

(defun init-closh ()
  (init-global define (make-instance 'closh-define)
               quote (make-instance 'closh-quote)
               set! (make-instance 'closh-set!)
               lambda (make-instance 'closh-lambda)
               begin (make-instance 'closh-begin)
               exit (make-instance 'closh-builtin
                                   :func #'closh-exit)))

(defun closh-repl ()
  (init-closh)
  (read-eval-print-loop))

