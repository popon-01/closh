(in-package :closh)

(define-class closh-op (closh-object))
(define-class closh-func (closh-op) penv args body)
(define-class closh-lambda (closh-func))
(define-class closh-macro (closh-func) (closh-macrop t))
(define-class op-lambda (closh-op))
(define-class op-quote (closh-op))
(define-class op-cond (closh-op))
(define-class op-and (closh-op))
(define-class op-or (closh-op))
(define-class op-begin (closh-op))

(defgeneric eval-closh-object (obj env))
(defmethod eval-closh-object ((const closh-const) (env closh-env))
  (declare (ignore env)) const)
(defmethod eval-closh-object ((sym closh-sym) (env closh-env))
  (get-env sym env))

(defun closh-eval (objects env &optional (ret nil))
  (if (null objects) ret
      (progn
        (let ((eval-car
               (eval-closh-object (car objects) env)))
          (closh-eval (cdr objects) env eval-car)))))


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

(defmethod bind-args ((cnil closh-nil) (argv closh-list)
                      (env closh-env))
  (declare (ignore cnil))
  (if-not (closh-null argv)
          (error "invalid number of arguments") env))


(defgeneric call-func (func argv env))

(defmethod call-func ((func closh-func) argv env)
  (eval-closh-object (body func)
                     (bind-args (args func) argv
                                (make-instance 'closh-local
                                               :parent (penv func)))))

(defgeneric macro-callp (obj env))

(defmethod macro-callp ((obj closh-object) env)
  (declare (ignore obj env)) nil)

(defmethod macro-callp ((lst closh-cons) (env closh-env))
  (and (symbolp (closh-car lst))
       (closh-macrop (get-env (closh-car lst) env))))

(defgeneric closh-macroexpand-1 (exp env))

(defmethod closh-macroexpand-1 ((exp closh-object) env)
  (declare (ignore env))
  (values exp nil))

(defmethod closh-macroexpand-1 ((lst closh-cons) (env closh-env))
  (if-not (macro-callp lst env)
          (values lst nil)
          (values (call-func (get-env (closh-car lst) env)
                             (closh-cdr lst) env)
                  t)))

(defgeneric closh-macroexpand (exp env))

(defmethod closh-macroexpand ((exp closh-exp) (env closh-env))
  (multiple-value-bind (ret callp) (closh-macroexpand-1 exp env)
    (if callp (closh-macroexpand ret env) ret)))


(defgeneric call-op (op argv env))

#|
(defmethod eval-closh-object ((lst closh-list) (env closh-env))
  (let ((expanded (closh-macroexpand lst env)))
    (call-op (get-env (closh-car expanded))
             (closh-cdr expanded) env)))



(defmethod call-op ((obj closh-object) argv env)
  (declare (ignore obj args env))
  (error "illeagal function call" (dump-to-str obj)))

(defmethod call-op ((macro-op closh-macro) argv env)
  (declare (ignore macro-op args env))
  (error "closh error: macro is not expanded"))

(defmethod call-op ((func closh-lambda) (argv closh-list)
                    (env closh-env))
  (call-func func (closh-map (lambda (exp) (eval-closh-object exp env))
                             argv)
             env))

(defmethod call-op ((op set!) (argv closh-list) (env closh-env))
  (update-env (closh-car argv) (closh-car (closh-cdr argv)) env))

(defmethod call-op ((op quote) (argv closh-cons) (env closh-env))
  (declare (ignore op env)) (closh-car argv))

|#
