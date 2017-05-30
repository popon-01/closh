(in-package :closh)

(defmethod call-op ((op closh-define) (argv closh-cons)
                    (env closh-env))
  (add-env (closh-car argv)
           (eval-closh-object (closh-nth 1 argv) env)
           env)
  (closh-car argv))

(defmethod call-op ((op closh-quote) (argv closh-cons)
                    (env closh-env))
  (closh-car argv))

(defmethod call-op ((op closh-set!) (argv closh-cons)
                    (env closh-env))
  (let ((val (eval-closh-object (closh-nth 1 argv) env)))
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
  (let ((args (closh-map #'closh-car (closh-car argv)))
        (vals (closh-map (lambda (bind) (closh-nth 1 bind))
                         (closh-car argv))))
    (call-op (make-instance 'closh-func
                            :penv env :args args
                            :body (make-closh-cons
                                   (make-instance 'closh-sym
                                                  :sym :begin)
                                   (closh-cdr argv)))
             vals env)))

(defmethod call-op ((op closh-let*) (argv closh-cons)
                    (env closh-env))
  (let ((binds (closh-car argv))
        (body (closh-cdr argv)))
    (if (closh-null binds)
        (call-op (make-instance 'closh-func
                                :penv env :args cnil
                                :body (make-closh-cons
                                       (make-instance 'closh-sym
                                                      :sym :begin)
                                       (closh-cdr argv)))
                 cnil env)
        (call-op (make-instance 'closh-func
                                :penv env
                                :args (make-closh-list
                                       (closh-nth 0 (closh-car binds)))
                                :body (make-closh-cons
                                       (make-instance 'closh-sym :sym :let*)
                                       (make-closh-cons
                                        (closh-cdr binds) body)))
                 (make-closh-list (closh-nth 1 (closh-car binds))) env))))

(defmethod call-op ((op closh-if) (argv closh-cons)
                    (env closh-env))
  (if (to-bool (eval-closh-object (closh-car argv) env))
      (eval-closh-object (closh-nth 1 argv) env)
      (eval-closh-object (closh-nth 2 argv) env)))

(defmethod call-op ((op closh-cond) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (clauses)
     (cond ((closh-null clauses) make-cnil)
           ((or (and (closh-symbolp (closh-car (closh-car clauses)))
                     (eq (sym (closh-car (closh-car clauses))) :else))
                (to-bool (eval-closh-object
                          (closh-car (closh-car clauses)) env)))                
            (eval-closh-object (make-closh-cons
                                (make-instance 'closh-sym :sym :begin)
                                (closh-cdr (closh-car clauses)))
                               env))
           (t (self (closh-cdr clauses)))))
   argv))

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
   argv cnil))


  


