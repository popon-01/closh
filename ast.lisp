(in-package :closh)

(define-class closh-ast ())
(defgeneric dump-to-str (ast))
(defmethod dump-to-str ((ast closh-ast))
  (class-name ast))

(define-class closh-const (closh-ast)
  value)
(defmethod dump-to-str ((const closh-const))
  (format nil "[const : ~a]" (value const)))

(define-class closh-sym (closh-ast)
  sym)
(defmethod dump-to-str ((sym closh-sym))
  (format nil "[symbol : ~a]" (sym sym)))
