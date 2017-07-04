(in-package :closh)

(defpackage :workspace
  (:use :cl))

(defun init-workspace ()
  (let ((ws (find-package :workspace)))
    (do-symbols (sym ws)
      (when (eq (symbol-package sym) ws)
        (makunbound sym)))))

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


(defgeneric cl->scheme (object))
(defmethod cl->scheme ((obj t))
  (error 'closh-to-scheme-error :obj obj))
(defmethod cl->scheme ((obj number))
  (make-instance 'closh-num :value obj))
(defmethod cl->scheme ((obj string))
  (make-instance 'closh-str :value obj))
(defmethod cl->scheme ((obj symbol))
  (if (eq obj t)
      (make-instance 'closh-bool :value t)
      (make-instance 'closh-sym :sym (intern (symbol-name obj)
                                             :keyword))))
(defmethod cl->scheme ((obj null)) cnil)
(defmethod cl->scheme ((obj cons))
  (make-instance 'closh-cons
                 :closh-car (cl->scheme (car obj))
                 :closh-cdr (cl->scheme (cdr obj))))


(defmacro closh-mode (&body body)
  (init-closh)
  (scheme->cl (closh-eval-seq (cl->scheme body) *global-enviroment*)))

