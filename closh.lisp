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
  (block repl
    (let ((line (raw-input "USER> ")))
      (handler-bind
          ((closh-exit-signal (lambda (c)
                                (declare (ignore c))
                                (format t "exit.~%")
                                (return-from repl))))
        (closh-print (closh-eval (closh-read line)
                                 *global-enviroment*))
        (read-eval-print-loop)))))
  
(defmacro add-global (&body key-and-values)
  `(progn
     ,@(iterate (for l on key-and-values by #'cddr)
                (collect
                    `(add-env ,(intern (symbol-name (first l))
                                       :keyword)
                              ,(second l)
                              *global-enviroment*)))))

(defmacro add-global-builtin (&body key-and-funcs)
  `(add-global
     ,@(iterate (for l on key-and-funcs by #'cddr)
                (nconcing
                 (list (first l)
                       `(make-instance 'closh-builtin
                                       :func ,(second l)))))))

(defun add-special ()
  (add-global define (make-instance 'closh-define)
              quote (make-instance 'closh-quote)
              set! (make-instance 'closh-set!)
              lambda (make-instance 'closh-lambda)
              let (make-instance 'closh-let)
              let* (make-instance 'closh-let*)
              if (make-instance 'closh-if)
              cond (make-instance 'closh-cond)
              or (make-instance 'closh-or)
              and (make-instance 'closh-and)
              begin (make-instance 'closh-begin)))


(defun add-number-func ()
  (add-global-builtin number?  #'closh-number?))

(defun add-list-func ()
  (add-global-builtin null? #'closh-null?
                      pair? #'closh-pair?
                      list? #'closh-list?
                      symbol #'closh-symbol?))

(defun add-bool-func ()
  (add-global-builtin boolean? #'closh-boolean?))

(defun add-str-func ()
  (add-global-builtin string? #'closh-string?))

(defun init-closh ()
  (setf *global-enviroment* (make-instance 'closh-global))
  (add-special)
  (add-number-func)
  (add-list-func)
  (add-bool-func)
  (add-str-func)
  (add-global-builtin exit #'closh-exit
                      procedure? #'closh-procedure?))

(defun closh-repl ()
  (init-closh)
  (read-eval-print-loop))

