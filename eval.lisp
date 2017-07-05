(in-package :closh)

(defmethod closh-eval-object ((const closh-const)
                              &optional (env *global-enviroment*))
  (declare (ignore env)) const)
(defmethod closh-eval-object ((lst closh-nil)
                              &optional (env *global-enviroment*))
  (declare (ignore env)) lst)
(defmethod closh-eval-object ((sym closh-sym)
                              &optional (env *global-enviroment*))
  (get-env sym env))
(defmethod closh-eval-object ((lst closh-cons)
                              &optional (env *global-enviroment*))
  (call-op (closh-eval-object (closh-car lst) env)
           (closh-cdr lst) env))

(defmethod closh-eval-all ((lst closh-list)
                           &optional (env *global-enviroment*))
  (closh-map (lambda (x) (closh-eval-object x env)) lst))

(defmethod closh-eval-seq ((lst closh-list)
                           &optional (env *global-enviroment*))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps)
         ret (self (closh-cdr exps)
                   (closh-eval-object (closh-car exps) env))))
   lst (make-instance 'closh-undef)))

