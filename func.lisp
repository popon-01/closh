(in-package :closh)

(defun call-func (func argv)
  (let ((newenv (make-instance 'closh-local
                               :parent (penv func))))
    (closh-eval-seq (body func)
                    (bind-args (args func) argv newenv))))

(defgeneric bind-args (sym argv env))
(defmethod bind-args ((obj closh-object) (argv closh-list)
                      (env closh-env))
  (error "~a is not symbol" (dump-to-str obj)))
(defmethod bind-args ((syms closh-cons) (argv closh-list)
                      (env closh-env))
  (if (null argv)
      (error "invalid number of arguments")
      (bind-args (closh-cdr syms) (closh-cdr argv)
                 (add-env (closh-car syms) (closh-car argv) env))))
(defmethod bind-args ((sym closh-sym) (val closh-list)
                      (env closh-env))
  (add-env sym val env))
(defmethod bind-args ((empty closh-nil) (argv closh-list)
                      (env closh-env))
  (declare (ignore empty))
  (if-not (closh-null argv)
          (error "invalid number of arguments") env))


;;///// lambda /////
(defmethod call-op ((func closh-func) (argv closh-list)
                    (env closh-env))
  (call-func func (closh-eval-all argv env)))

;;///// macros /////
(defmethod macro-callp ((obj closh-object)
                        &key (env *global-enviroment*))
  (declare (ignore env)) nil)
(defmethod macro-callp ((lst closh-cons)
                        &key (env *global-enviroment*))
  (and (symbolp (closh-car lst))
       (closh-macrop (get-env (closh-car lst) env))))

(defmethod closh-macroexpand-1 ((exp closh-object)
                                &key (env *global-enviroment*))
  (declare (ignore env)) (values exp nil))

(defmethod closh-macroexpand-1 ((lst closh-cons)
                                &key (env *global-enviroment*))
  (if-not (macro-callp lst :env env)
          (values lst nil)
          (values (call-func (get-env (closh-car lst) env)
                             (closh-cdr lst))
                             t)))

(defmethod closh-macroexpand ((exp closh-exp)
                              &key (env *global-enviroment*))
  (multiple-value-bind (ret callp) (closh-macroexpand-1 exp :env env)
    (if callp (closh-macroexpand ret :env env) ret)))

(defmethod call-op ((macro-op closh-macro) (argv closh-list)
                    (env closh-env))
  (closh-eval-object (call-func macro-op argv) env))

