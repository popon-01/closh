(in-package :closh)

(defmethod closh-eval-object ((const closh-const) (env closh-env))
  (declare (ignore env)) const)
(defmethod closh-eval-object ((sym closh-sym) (env closh-env))
  (get-env sym env))
(defmethod closh-eval-object ((lst closh-cons) (env closh-env))
  (call-op (closh-eval-object (closh-car lst) env)
           (closh-cdr lst) env))

(defmethod closh-eval-all ((lst closh-list) (env closh-env))
  (closh-map (lambda (x) (closh-eval-object x env)) lst))

(defun closh-eval (objects env &optional (ret nil))
  (if (closh-null objects) ret
      (let ((eval-car
             (closh-eval-object (closh-car objects) env)))
        (closh-eval (closh-cdr objects) env eval-car))))

