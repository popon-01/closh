(in-package :closh)

(defpackage :workspace (:use :cl))

(defun init-workspace ()
  (let ((ws (find-package :workspace)))
    (do-symbols (sym ws)
      (when (eq (symbol-package sym) ws)
        (makunbound sym)
        (fmakunbound sym)))))

(defgeneric scheme->cl (object))
(defmethod scheme->cl ((obj closh-object))
  (error 'closh-from-scheme-error :obj obj))
(defmethod scheme->cl ((obj closh-const))
  (value obj))
(defmethod scheme->cl ((obj closh-sym))
  (intern (symbol-name (sym obj)) (find-package :workspace)))
(defmethod scheme->cl ((obj closh-nil)) nil)
(defmethod scheme->cl ((obj closh-cons))
  (cons (scheme->cl (closh-car obj)) (scheme->cl (closh-cdr obj))))
(defmethod scheme->cl ((obj closh-func))
  (lambda (&rest x)
    (scheme->cl
     (call-op obj (cl->scheme x)))))

(defgeneric cl->scheme (object))
(defmethod cl->scheme ((obj t))
  (error 'closh-to-scheme-error :obj obj))
(defmethod cl->scheme ((obj closh-object)) obj)
(defmethod cl->scheme ((obj number))
  (make-instance 'closh-num :value obj))
(defmethod cl->scheme ((obj string))
  (make-instance 'closh-str :value obj))
(defmethod cl->scheme ((obj symbol))
  (cond ((eq obj t) (make-instance 'closh-bool :value t))
        ((eq obj '\#t) (make-instance 'closh-bool :value t))
        ((eq obj '\#f) (make-instance 'closh-bool :value nil))
        (t (make-instance 'closh-sym :sym (intern (symbol-name obj)
                                                  :keyword)))))
(defmethod cl->scheme ((obj null)) cnil)
(defmethod cl->scheme ((obj cons))
  (make-instance 'closh-cons
                 :closh-car (cl->scheme (car obj))
                 :closh-cdr (cl->scheme (cdr obj))))

(defmethod cl->scheme ((obj function))
  (make-instance 'closh-builtin
                 :func (lambda (&rest argv)
                         (cl->scheme
                          (apply obj (mapcar #'scheme->cl argv))))))


(defmacro closh-mode (&body body)
  (init-closh)
  (scheme->cl (closh-eval-seq (cl->scheme body))))

(defmethod call-op ((op closh-cl-mode) (argv closh-list)
                    &optional (env *global-enviroment*))
  (declare (ignore env))
  (unwind-protect
       (handler-bind ((condition (lambda (c) (declare (ignore c))
                                         (error 'closh-cl-mode-error))))
         (let ((ret nil))
           (init-workspace)
           (in-package :workspace)
           (setf ret (cl->scheme
                      (eval (cons 'progn (scheme->cl argv)))))
           ret))
    (in-package :closh)))

