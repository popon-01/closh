(in-package :closh)

(defun call-func (func argv)
  (let ((newenv (make-instance 'closh-local
                               :parent (penv func))))
    (closh-eval-object (body func)
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
(defgeneric macro-callp (obj env))
(defmethod macro-callp ((obj closh-object) env) nil)
(defmethod macro-callp ((lst closh-cons) (env closh-env))
  (and (symbolp (closh-car lst))
       (closh-macrop (get-env (closh-car lst) env))))

(defgeneric closh-macroexpand-1 (exp env))
(defmethod closh-macroexpand-1 ((exp closh-object) env)
  (values exp nil))
(defmethod closh-macroexpand-1 ((lst closh-cons) (env closh-env))
  (if-not (macro-callp lst env)
          (values lst nil)
          (values (call-func (get-env (closh-car lst) env)
                             (closh-cdr lst))
                             t)))

(defgeneric closh-macroexpand (exp env))
(defmethod closh-macroexpand ((exp closh-exp) (env closh-env))
  (multiple-value-bind (ret callp) (closh-macroexpand-1 exp env)
    (if callp (closh-macroexpand ret env) ret)))

(defmethod call-op ((macro-op closh-macro) (argv closh-list)
                    (env closh-env))
  (closh-eval-object (call-func macro-op argv) env))

