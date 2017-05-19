(in-package :closh)

(define-class closh-env () (table (make-hash-table)))
(define-class closh-global (closh-env))
(define-class closh-local (closh-env) parent)

(defgeneric add-value (sym value env))
(defgeneric get-value (sym env))
(defgeneric update-value (sym value env))

(defmethod add-value ((sym closh-sym) (value closh-object)
                      (env closh-env))
  (setf (gethash (sym sym) (table env)) value) env)


(defmethod get-value ((sym closh-sym) (env closh-global))
  (multiple-value-bind (val foundp) (gethash (sym sym) (table env))
    (if foundp val (error "symbol ~a is unbound." (symbol-name (sym sym))))))

(defmethod get-value ((sym closh-sym) (env closh-local))
  (multiple-value-bind (val foundp) (gethash (sym sym) (table env))
    (if foundp val (get-value sym (parent env)))))


(defmethod update-value ((sym closh-sym) (value closh-object)
                         (env closh-global))
  (multiple-value-bind (old foundp) (gethash (sym sym) (table env))
    (declare (ignore old))
    (if foundp
        (progn (setf (gethash (sym sym) (table env)) value) env)
        (error "symbol ~a is unbound." (symbol-name (sym sym))))))

(defmethod update-value ((sym closh-sym) (value closh-object)
                         (env closh-local))
  (multiple-value-bind (old foundp) (gethash (sym sym) (table env))
    (declare (ignore old))
    (if foundp
        (progn (setf (gethash (sym sym) (table env)) value) env)
        (update-value sym value (parent env)))))




