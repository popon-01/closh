(in-package :closh)

(defmethod eval-closh-object ((const closh-const) (env closh-env))
  (declare (ignore env)) const)
(defmethod eval-closh-object ((sym closh-sym) (env closh-env))
  (get-env sym env))
(defmethod eval-closh-object ((lst closh-cons) (env closh-env))
  (call-op (eval-closh-object (closh-car lst) env)
           (closh-cdr lst) env))

(defun closh-eval (objects env &optional (ret nil))
  (if (null objects) ret
      (progn
        (let ((eval-car
               (eval-closh-object (car objects) env)))
          (closh-eval (cdr objects) env eval-car)))))

