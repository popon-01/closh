(in-package :closh)

(defun make-body (body)
  (make-closh-cons
   (make-instance 'closh-sym :sym :begin) body))

;;define
(defmethod call-op ((op closh-define) (argv closh-cons)
                    (env closh-env))
  (if (closh-listp (closh-car argv))
      (call-define-func argv env)
      (call-define-var argv env)))

(defun call-define-var (argv env)
  (add-env (closh-car argv)
           (eval-closh-object (closh-nth 1 argv) env)
           env)
  (closh-car argv))

(defun call-define-func (argv env)
  (add-env (closh-car (closh-nth 0 argv))
           (make-instance 'closh-func
                          :penv env
                          :args (closh-cdr (closh-nth 0 argv))
                          :body (make-body (closh-cdr argv)))
           env)
  (closh-car (closh-nth 0 argv)))


;;defmacro
(defmethod call-op ((op closh-defmacro) (argv closh-cons)
                    (env closh-env))
  (add-env (closh-car (closh-nth 0 argv))
           (make-instance 'closh-macro
                          :penv env
                          :args (closh-cdr (closh-nth 0 argv))
                          :body (make-body (closh-cdr argv)))
           env)
  (closh-car (closh-nth 0 argv)))

;;quote
(defmethod call-op ((op closh-quote) (argv closh-cons)
                    (env closh-env))
  (closh-car argv))

;;set!
(defmethod call-op ((op closh-set!) (argv closh-cons)
                    (env closh-env))
  (let ((val (eval-closh-object (closh-nth 1 argv) env)))
    (update-env (closh-car argv) val env)
    val))

;lambda
(defmethod call-op ((op closh-lambda) (argv closh-cons)
                    (env closh-env))
  (make-instance 'closh-func
                 :penv env
                 :args (closh-car argv)
                 :body (make-body (closh-cdr argv))))

;;let
(defmethod call-op ((op closh-let) (argv closh-cons)
                    (env closh-env))
  (if (closh-symbolp (closh-car argv))
      (call-let (closh-cdr argv) env (closh-car argv))
      (call-let argv env)))

(defun call-let (argv env &optional (namesym nil))
  (let* ((args (closh-map (lambda (bind) (closh-nth 0 bind))
                          (closh-car argv)))
         (vals (closh-map (lambda (bind) (closh-nth 1 bind))
                          (closh-car argv)))
         (func (make-instance 'closh-func
                              :penv env :args args
                              :body (make-body (closh-cdr argv)))))
      (when namesym (add-env namesym func env))
      (call-op func vals env)))


(defmethod call-op ((op closh-let*) (argv closh-cons)
                    (env closh-env))
  (let ((binds (closh-car argv))
        (body (closh-cdr argv)))
    (if (closh-null binds)
        (call-op (make-instance 'closh-func
                                :penv env :args cnil
                                :body (make-body (closh-cdr argv)))
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

(defmethod call-op ((op closh-letrec) (argv closh-cons)
                    (env closh-env))
  (let* ((args (closh-map (lambda (bind) (closh-nth 0 bind))
                          (closh-car argv)))
         (vals (closh-map (lambda (bind) (closh-nth 1 bind))
                          (closh-car argv)))
         (nenv (funcall (alambda (syms env)
                          (if (closh-null syms) env
                              (self (closh-cdr syms)
                                    (add-env (closh-car syms)
                                             (make-instance 'closh-undef)
                                             env))))
                        args (make-instance 'closh-local :parent env)))
         (nenv2 (bind-args args (closh-map (lambda (v) (eval-closh-object v nenv))
                                           vals)
                           nenv))
         (func (make-instance 'closh-func
                              :penv nenv2 :args cnil
                              :body (make-body (closh-cdr argv)))))
    (call-op func cnil nenv2)))

;;if
(defmethod call-op ((op closh-if) (argv closh-cons)
                    (env closh-env))
  (if (to-bool (eval-closh-object (closh-car argv) env))
      (eval-closh-object (closh-nth 1 argv) env)
      (eval-closh-object (closh-nth 2 argv) env)))

;;cond
(defmethod call-op ((op closh-cond) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (clauses)
     (cond ((closh-null clauses) make-cnil)
           ((or (and (closh-symbolp (closh-car (closh-car clauses)))
                     (eq (sym (closh-car (closh-car clauses))) :else))
                (to-bool (eval-closh-object
                          (closh-car (closh-car clauses)) env)))                
            (eval-closh-object (make-body
                                (closh-cdr (closh-car clauses)))
                               env))
           (t (self (closh-cdr clauses)))))
   argv))

;;or
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

;;and
(defmethod call-op ((op closh-and) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps) ret
         (let ((val (eval-closh-object (closh-car exps) env)))
           (if-not (to-bool val) val
                   (self (closh-cdr exps) val)))))
   argv (make-instance 'closh-bool :value t)))


;;begin
(defmethod call-op ((op closh-begin) (argv closh-cons)
                    (env closh-env))
  (funcall
   (alambda (exps ret)
     (if (closh-null exps)
         ret (self (closh-cdr exps)
                   (eval-closh-object (closh-car exps) env))))
   argv cnil))

  


