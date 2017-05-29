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

(defmethod call-op ((op closh-let) (argv closh-cons)
                    (env closh-env))
  (let ((syms (closh-map #'closh-car (closh-car argv)))
        (vals (closh-map (lambda (bind)
                           (closh-car (closh-cdr bind)))
                         (closh-car argv))))
    (eval-closh-object (make-closh-cons
                        (make-instance 'closh-sym :sym :begin)
                        (closh-cdr argv))
                       (funcall
                        (alambda (syms vals local)
                          (if (closh-null syms) local
                              (let ((val (eval-closh-object
                                          (closh-car vals) env)))
                                (self (closh-cdr syms)
                                      (closh-cdr vals)
                                      (add-env (closh-car syms) val
                                               local)))))
                        syms vals
                        (make-instance 'closh-local :parent env)))))

(defmethod call-op ((op closh-let*) (argv closh-cons)
                    (env closh-env))
  (let ((syms (closh-map #'closh-car (closh-car argv)))
        (vals (closh-map (lambda (bind)
                           (closh-car (closh-cdr bind)))
                         (closh-car argv))))
    (eval-closh-object (make-closh-cons
                        (make-instance 'closh-sym :sym :begin)
                        (closh-cdr argv))
                       (funcall
                        (alambda (syms vals local)
                          (if (closh-null syms) local
                              (let* ((newenv (make-instance 'closh-local
                                                            :parent env))
                                     (val (eval-closh-object
                                           (closh-car vals) local)))
                                (self (closh-cdr syms)
                                      (closh-cdr vals)
                                      (add-env (closh-car syms) val
                                                newenv)))))
                        syms vals
                        (make-instance 'closh-local :parent env)))))


(defmethod call-op ((op closh-or) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (exps)
     (if (closh-null exps)
         (make-instance 'closh-bool :value nil)
         (let ((val (eval-closh-object (closh-car exps) env)))
           (if (to-bool val) val
               (self (closh-cdr exps))))))
   argv))

(defmethod call-op ((op closh-and) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps) ret
         (let ((val (eval-closh-object (closh-car exps) env)))
           (if-not (to-bool val) val
                   (self (closh-cdr exps) val)))))
   argv (make-instance 'closh-bool :value t)))

(defmethod call-op ((op closh-begin) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps)
         ret (self (closh-cdr exps)
                   (eval-closh-object (closh-car exps) env))))
   argv (make-instance 'closh-nil)))


  


