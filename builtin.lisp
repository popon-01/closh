(in-package :closh)

(defmethod call-op ((op closh-builtin) (argv closh-list)
                    &optional (env *global-enviroment*))
  (handler-bind ((type-error
                  (lambda (c) (declare (ignore c))
                          (error 'closh-type-error)))
                 (simple-error
                  (lambda (c) (declare (ignore c))
                          (error 'closh-type-error))))
    (apply (func op) (unpack-closh-list (closh-eval-all argv env)))))

(defun closh-exit (&rest argv)
  (declare (ignore argv))
  (signal 'closh-exit-signal))

(defun closh-type-pred (pred)
  (lambda (obj)
    (make-instance 'closh-bool
                   :value (funcall pred obj))))

(defun closh-num-op (op return-type)
  (lambda (&rest argv)
    (let ((argnum (mapcar #'value argv)))
      (make-instance return-type
                     :value (apply op argnum)))))

(defun closh-num-calc (op)
  (closh-num-op op 'closh-num))

(defun closh-num-pred (op)
  (closh-num-op op 'closh-bool))

(defun closh-not (arg)
  (make-instance 'closh-bool
                 :value (not (to-bool arg))))

(defun closh-string-append (&rest argv)
  (let ((argstr (mapcar #'value argv)))
    (make-instance 'closh-str
                   :value (apply #'concatenate
                                 'string argstr))))

(defun closh-symbol->string (arg)
  (make-instance 'closh-str
                 :value (symbol-name (sym arg))))
(defun closh-string->symbol (arg)
  (make-instance 'closh-sym
                 :sym (intern (string-upcase (value arg)) :keyword)))
(defun closh-number->string (arg)
  (make-instance 'closh-str
                 :value (princ-to-string (value arg))))
(defun closh-string->number (arg)
  (make-instance 'closh-num
                 :value (read-from-string (value arg))))

(defgeneric closh-eq? (arg1 arg2))
(defmethod closh-eq? ((arg1 closh-object) (arg2 closh-object))
  (make-instance 'closh-bool :value nil))
(defmethod closh-eq? ((arg1 closh-const) (arg2 closh-const))
  (make-instance 'closh-bool :value (eq (value arg1) (value arg2))))
(defmethod closh-eq? ((arg1 closh-nil) (arg2 closh-nil))
  (make-instance 'closh-bool :value t))
(defmethod closh-eq? ((arg1 closh-sym) (arg2 closh-sym))
  (make-instance 'closh-bool :value (eq (sym arg1) (sym arg2))))

(defun closh-neq? (arg1 arg2)
  (closh-not (closh-eq? arg1 arg2)))

(defgeneric closh-equal? (arg1 arg2))
(defmethod closh-equal? ((arg1 closh-object) (arg2 closh-object))
  (closh-eq? arg1 arg2))
(defmethod closh-equal? ((arg1 closh-cons) (arg2 closh-cons))
  (make-instance 'closh-bool
                 :value (and (value (closh-equal? (closh-car arg1)
                                                  (closh-car arg2)))
                             (value (closh-equal? (closh-cdr arg1)
                                                  (closh-cdr arg2))))))
(defmethod closh-equal? ((arg1 closh-str) (arg2 closh-str))
  (make-instance 'closh-bool :value (string= (value arg1) (value arg2))))
(defmethod closh-equal? ((arg1 closh-num) (arg2 closh-num))
  (make-instance 'closh-num :value (= (value arg1) (value arg2))))

(defun closh-print (obj &optional (stream t))
  (format stream "~a~%" (dump-to-str obj)) obj)
