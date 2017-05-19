(in-package :closh)

(define-class closh-op (closh-object))
(define-class closh-lambda (closh-op) env body)
(define-class closh-macro (closh-op) body)
(define-class op-lambda (closh-op))
(define-class op-quote (closh-op))
(define-class op-if (closh-op))
(define-class op-and (closh-op))
(define-class op-or (closh-op))
(define-class op-begin (closh-op))

(defmethod bind-args ((syms closh-cons) (vals closh-list)
                      (env closh-env))
  (if (null vals)
      (error "invalid number of arguments")
      (bind-args (closh-cdr syms) (closh-cdr vals)
                 (add-value (closh-car syms) (closh-car vals) env))))

(defmethod bind-args ((sym closh-sym) (val closh-list)
                      (env closh-env))
  (add-value sym val env))

(defmethod bind-args ((cnil closh-nil) (vals closh-list)
                      (env closh-env))
  (declare (ignore cnil))
  (if-not (closh-null vals)
          (error "invalid number of arguments") env))

#|
(defgeneric closh-eval (obj env))

(defmethod closh-eval ((const closh-const) (env closh-env))
  (declare (ignore env)) (value const))

(defmethod closh-eval ((sym closh-sym) (env closh-env))
  (getvalue sym env))

(defmethod closh-eval ((lst closh-cons) (env closh-env))
  (call-op (closh-car lst) env) (closh-cdr value))

(defgeneric call-op (op args env))

(defmethod call-op ((obj closh-object) (args closh-cons) (env closh-env))
  (declare (ignore obj args env))
  (error "object ~a is not callable" (dump-to-str obj)))

(defmethod call-op ((op quote) (args closh-cons) (env closh-env))
  (declare (ignore op env))
  (if-not (= (closh-length args) 1)
          (error "invalid syntax") args))
|#
