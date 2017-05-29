(in-package :closh)

(defmethod call-op ((op closh-define) (argv closh-cons)
                    (env closh-env))
  (add-env (closh-car argv)
           (eval-closh-object (closh-car (closh-cdr argv)) env)
           env)
  (closh-car argv))

(defmethod call-op ((op closh-quote) (argv closh-cons)
                    (env closh-env))
  (closh-car argv))

(defmethod call-op ((op closh-set!) (argv closh-cons)
                    (env closh-env))
  (let ((val (eval-closh-object
              (closh-car (closh-cdr argv)) env)))
    (update-env (closh-car argv) val env)
    val))

(defmethod call-op ((op closh-lambda) (argv closh-cons)
                    (env closh-env))
  (make-instance 'closh-func
                 :penv env
                 :args (closh-car argv)
                 :body (make-closh-cons
                        (make-instance 'closh-sym :sym :begin)
                        (closh-cdr argv))))

(defmethod call-op ((op closh-begin) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps)
         ret (self (closh-cdr exps)
                   (eval-closh-object (closh-car exps) env))))
   argv (make-instance 'closh-nil)))


  


