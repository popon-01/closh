(in-package :closh)

(defmethod closh-eval-object ((const closh-const) (env closh-env)) const)
(defmethod closh-eval-object ((lst closh-nil) (env closh-env)) lst)
(defmethod closh-eval-object ((sym closh-sym) (env closh-env))
  (get-env sym env))
(defmethod closh-eval-object ((lst closh-cons) (env closh-env))
  (call-op (closh-eval-object (closh-car lst) env)
           (closh-cdr lst) env))

(defmethod closh-eval-all ((lst closh-list) (env closh-env))
  (closh-map (lambda (x) (closh-eval-object x env)) lst))

(defmethod closh-eval-seq ((lst closh-list) (env closh-env))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps)
         ret (self (closh-cdr exps)
                   (closh-eval-object (closh-car exps) env))))
   lst (make-instance 'closh-undef)))

