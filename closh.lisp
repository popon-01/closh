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

(defun init-closh ()
  (setf *global-enviroment* (make-instance 'closh-global)))

(defun closh-repl ()
  (init-closh)
  (when (read-eval-print)
    (closh-repl)))

