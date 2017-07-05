(in-package :closh)

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output)
  (read-line *standard-input*))

(defun read-eval-print-loop ()
  (block outer-repl
    (let ((line (raw-input "USER> ")))
      (block inner-repl
        (handler-bind ((closh-exit-signal
                        (lambda (c) (declare (ignore c))
                                (format t "exit.~%") (force-output)
                                (return-from outer-repl)))
                       (yacc-parse-error
                        (lambda (c) (declare (ignore c))
                                (format t "[closh-error] parse failed : ~a~%"
                                        line)
                                (force-output)
                                (return-from inner-repl)))
                       (closh-error
                        (lambda (c) (handle-error c)
                                (return-from inner-repl))))
          (closh-print
           (closh-eval-seq
            (closh-map #'check-toplevel
                       (closh-read line)))))))
    (read-eval-print-loop)))  

(defun closh-repl ()
  (init-closh)
  (read-eval-print-loop))

