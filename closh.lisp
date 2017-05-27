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

(defun read-eval-print ()
  (let ((line (raw-input "USER> ")))
    (when (string/= line "(quit)")
      (closh-print (closh-eval (closh-read line)
                               *global-enviroment*))
      t)))


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
  (init-global quote (make-instance 'op-quote)))

(defun closh-repl ()
  (init-closh)
  (when (read-eval-print)
    (closh-repl)))

