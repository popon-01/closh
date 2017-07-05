(in-package :closh)

(defun make-body (body)
  (make-closh-cons
   (make-instance 'closh-sym :sym :begin) body))

;;define
(defmethod call-op ((op closh-define) (argv closh-list)
                    &optional (env *global-enviroment*))
  (if (closh-listp (closh-car argv))
      (call-define-func argv env)
      (call-define-var argv env)))

(defun call-define-var (argv env)
  (add-env (closh-car argv)
           (closh-eval-object (closh-nth 1 argv) env)
           env)
  (closh-car argv))

(defun call-define-func (argv env)
  (add-env (closh-car (closh-nth 0 argv))
           (make-instance 'closh-func
                          :penv env
                          :args (closh-cdr (closh-nth 0 argv))
                          :body (closh-cdr argv))
           env)
  (closh-car (closh-nth 0 argv)))


;;defmacro
(defmethod call-op ((op closh-defmacro) (argv closh-list)
                    &optional (env *global-enviroment*))
  (add-env (closh-car (closh-nth 0 argv))
           (make-instance 'closh-macro
                          :penv env
                          :args (closh-cdr (closh-nth 0 argv))
                          :body (closh-cdr argv))
           *global-enviroment*)
  (closh-car (closh-nth 0 argv)))

;;quote
(defmethod call-op ((op closh-quote) (argv closh-list)
                    &optional (env *global-enviroment*))
  (closh-car argv))

;;set!
(defmethod call-op ((op closh-set!) (argv closh-list)
                    &optional (env *global-enviroment*))
  (let ((val (closh-eval-object (closh-nth 1 argv) env)))
    (update-env (closh-car argv) val env)
    val))

;;lambda
(defmethod call-op ((op closh-lambda) (argv closh-list)
                    &optional (env *global-enviroment*))
  (make-instance 'closh-func
                 :penv env
                 :args (closh-car argv)
                 :body (closh-cdr argv)))

;;let
(defmethod call-op ((op closh-let) (argv closh-list)
                    &optional (env *global-enviroment*))
  (if (closh-symbolp (closh-car argv))
      (call-let (closh-cdr argv) env (closh-car argv))
      (call-let argv env)))

(defun call-let (argv env &optional (namesym nil))
  (let* ((args (closh-nth-all 0 (closh-car argv)))
         (vals (closh-nth-all 1 (closh-car argv)))
         (func (make-instance 'closh-func
                              :penv env :args args
                              :body (closh-cdr argv))))
      (when namesym (add-env namesym func env))
      (call-op func vals env)))

;;let*
(defmethod call-op ((op closh-let*) (argv closh-list)
                    &optional (env *global-enviroment*))
  (let ((binds (closh-car argv))
        (body (closh-cdr argv)))
    (if (closh-null binds)
        (call-op (make-instance 'closh-func
                                :penv env :args cnil
                                :body (closh-cdr argv))
                 cnil env)
        (closh-eval-object
         (make-closh-cons (make-instance 'closh-sym :sym :let*)
                          (make-closh-cons
                           (closh-cdr binds) body))
         (add-env (closh-nth 0 (closh-car binds))
                  (closh-eval-object
                   (closh-nth 1 (closh-car binds)) env)
                  (make-instance 'closh-local :parent env))))))

;;letrec
(defmethod call-op ((op closh-letrec) (argv closh-list)
                    &optional (env *global-enviroment*))
  (let* ((args (closh-nth-all 0 (closh-car argv)))
         (vals (closh-nth-all 1 (closh-car argv)))
         (nenv (funcall (alambda (syms env) ; init all symbol with #<undef> 
                          (if (closh-null syms) env
                              (self (closh-cdr syms)
                                    (add-env (closh-car syms)
                                             (make-instance 'closh-undef)
                                             env))))
                        args (make-instance 'closh-local :parent env)))
         (nenv2 (bind-args args (closh-eval-all vals nenv) nenv))
         (func (make-instance 'closh-func
                              :penv nenv2 :args cnil
                              :body (closh-cdr argv))))
    (call-op func cnil nenv2)))

;;if
(defmethod call-op ((op closh-if) (argv closh-list)
                    &optional (env *global-enviroment*))
  (if (to-bool (closh-eval-object (closh-car argv) env))
      (closh-eval-object (closh-nth 1 argv) env)
      (closh-eval-object (closh-nth 2 argv) env)))

;;cond
(defmethod call-op ((op closh-cond) (argv closh-list)
                    &optional (env *global-enviroment*))
  (funcall
   (alambda (clauses)
     (cond ((closh-null clauses) cnil)
           ((or (and (closh-symbolp (closh-car (closh-car clauses)))
                     (eq (sym (closh-car (closh-car clauses))) :else))
                (to-bool (closh-eval-object
                          (closh-car (closh-car clauses)) env)))                
            (closh-eval-seq (closh-cdr (closh-car clauses))
                            env))
           (t (self (closh-cdr clauses)))))
   argv))

;;or
(defmethod call-op ((op closh-or) (argv closh-list)
                    &optional (env *global-enviroment*))
  (funcall
   (alambda (exps)
     (if (closh-null exps)
         (make-instance 'closh-bool :value nil)
         (let ((val (closh-eval-object (closh-car exps) env)))
           (if (to-bool val) val
               (self (closh-cdr exps))))))
   argv))

;;and
(defmethod call-op ((op closh-and) (argv closh-list)
                    &optional (env *global-enviroment*))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps) ret
         (let ((val (closh-eval-object (closh-car exps) env)))
           (if-not (to-bool val) val
                   (self (closh-cdr exps) val)))))
   argv (make-instance 'closh-bool :value t)))


;;begin
(defmethod call-op ((op closh-begin) (argv closh-list)
                    &optional (env *global-enviroment*))
  (closh-eval-seq argv env))

;;do
(defmethod call-op ((op closh-do) (argv closh-list)
                    &optional (env *global-enviroment*))
  (let ((syms (closh-nth-all 0 (closh-nth 0 argv)))
        (inits (closh-nth-all 1 (closh-nth 0 argv)))
        (updates (closh-nth-all 2 (closh-nth 0 argv)))
        (test (closh-car (closh-nth 1 argv)))
        (ret (closh-cdr (closh-nth 1 argv)))
        (body (closh-nthcdr 2 argv)))
    (funcall
     (alambda (inits env)
       (let ((nenv (bind-args syms (closh-eval-all inits env)
                              (make-instance 'closh-local :parent env))))
         (if (to-bool (closh-eval-object test nenv))
             (closh-eval-seq ret nenv)
             (progn
               (closh-eval-seq body nenv)
               (self updates nenv)))))
     inits env)))



