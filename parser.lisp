(in-package :closh)

(define-class lexer-stream ()
  lexer (hold nil))

(defgeneric look-ahead (stream))
(defmethod look-ahead ((stream lexer-stream))
  (when (null (hold stream))
    (multiple-value-bind (tag val) (funcall (lexer stream))
      (setf (hold stream) (cons tag val))))
  (values (car (hold stream)) (cdr (hold stream))))

(defgeneric consume-token (stream))
(defmethod consume-token ((stream lexer-stream))
  (aif (hold stream)
       (progn
         (setf (hold stream) nil)
         (values (car it) (cdr it)))
       (funcall (lexer stream))))
