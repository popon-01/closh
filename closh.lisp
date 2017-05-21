(in-package :closh)

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output)
  (read-line *standard-input*))

(defun closh-read (str)
  (parse-with-lexer (closh-lexer str) closh-parser))

(defun read-eval-print ()
  (let ((line (raw-input "USER> ")))
    (closh-print (closh-eval (closh-read line)))))

(defun closh-repl ()
  (when (read-eval-print)
    (closh-repl)))

