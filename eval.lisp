(in-package :closh)

(define-class closh-op (closh-object))
(define-class closh-lambda (closh-op) penv args body)
(define-class closh-macro (closh-op) penv args body)
(define-class op-lambda (closh-op))
(define-class op-quote (closh-op))
(define-class op-cond (closh-op))
(define-class op-and (closh-op))
(define-class op-or (closh-op))
(define-class op-begin (closh-op))

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

;; want to macroexpand, without evaluation
;; body of lambda, debugging macro 
(defgeneric closh-macroexpand ())

#|
(defgeneric closh-eval (obj env))
(defmethod closh-eval ((const closh-const) (env closh-env))
  (declare (ignore env)) (value const))
(defmethod closh-eval ((sym closh-sym) (env closh-env))
  (get-env sym env))
(defmethod closh-eval ((lst closh-list) (env closh-env))
  (call-op (get-env (closh-car lst)) (closh-cdr lst) env))

(defgeneric call-op (op argv env))

(defmethod call-op ((obj closh-object) argv env)
  (declare (ignore obj args env))
  (error "illeagal function call" (dump-to-str obj)))

(defmethod call-op ((op quote) (argv closh-cons) (env closh-env))
  (declare (ignore op env))
  (if-not (= (closh-length args) 1)
          (error "invalid syntax") args))

|#
