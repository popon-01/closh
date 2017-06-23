(in-package :closh)

(define-class closh-env () (table (make-hash-table)))
(define-class closh-global (closh-env))
(define-class closh-local (closh-env) parent)

(defgeneric add-env (sym value env))
(defgeneric get-env (sym env))
(defgeneric update-env (sym value env))

(defmethod add-env ((sym closh-sym) (value closh-object)
                    (env closh-env))
  (setf (gethash (sym sym) (table env)) value) env)

;; for init
(defmethod add-env ((sym symbol) (value closh-object)
                    (env closh-env))
  (setf (gethash sym (table env)) value) env)


(defmethod get-env ((sym closh-sym) (env closh-global))
  (multiple-value-bind (val foundp) (gethash (sym sym) (table env))
    (if foundp val (error 'closh-unbound-error
                          :symname (symbol-name (sym sym))))))

(defmethod get-env ((sym closh-sym) (env closh-local))
  (multiple-value-bind (val foundp) (gethash (sym sym) (table env))
    (if foundp val (get-env sym (parent env)))))


(defmethod update-env ((sym closh-sym) (value closh-object)
                         (env closh-global))
  (multiple-value-bind (old foundp) (gethash (sym sym) (table env))
    (declare (ignore old))
    (if foundp
        (progn (setf (gethash (sym sym) (table env)) value) env)
        (error 'closh-unbound-error :symname (symbol-name (sym sym))))))

(defmethod update-env ((sym closh-sym) (value closh-object)
                         (env closh-local))
  (multiple-value-bind (old foundp) (gethash (sym sym) (table env))
    (declare (ignore old))
    (if foundp
        (progn (setf (gethash (sym sym) (table env)) value) env)
        (update-env sym value (parent env)))))




