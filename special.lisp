(in-package :closh)

(defmethod call-op ((op op-define) (argv closh-cons)
                    (env closh-env))
  (add-env (closh-car argv)
           (eval-closh-object (closh-car (closh-cdr argv)) env)
           env)
  (closh-car argv))

(defmethod call-op ((op op-quote) (argv closh-cons)
                    (env closh-env))
  (closh-car argv))

(defmethod call-op ((op op-set!) (argv closh-cons)
                    (env closh-env))
  (update-env (closh-car argv)
              (eval-closh-object (closh-car (closh-cdr argv)) env)
              env)
  (closh-car argv))


