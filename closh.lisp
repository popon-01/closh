(in-package :closh)

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output)
  (read-line *standard-input*))

(defun closh-read (str) str)

(defun closh-eval (ast) ast)

(defun closh-print (exp) exp)

(defun read-eval-print ()
  (let ((line (raw-input "USER> ")))
    (closh-print (closh-eval (closh-read line)))))

(defun closh-repl ()
  (when (read-eval-print)
    (closh-repl)))

