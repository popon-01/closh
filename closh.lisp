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
               or (make-instance 'closh-or)
               and (make-instance 'closh-and)
               begin (make-instance 'closh-begin)
               exit (make-instance 'closh-builtin
                                   :func #'closh-exit)))

(defun closh-repl ()
  (init-closh)
  (read-eval-print-loop))

