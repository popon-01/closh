(in-package :closh)

(defmethod call-op ((op closh-builtin) (argv closh-list)
                    (env closh-env))
  (apply (func op) (revert-closh-list
                    (closh-map (lambda (exp) (eval-closh-object exp env))
                               argv))))

(defun closh-exit (&rest argv)
  (declare (ignore argv))
  (signal 'closh-exit-signal))

(defun closh-number? (obj)
  (make-instance 'closh-bool
                 :value (closh-numberp obj)))

(defun closh-null? (obj)
  (make-instance 'closh-bool
                 :value (closh-null obj)))

(defun closh-pair? (obj)
  (make-instance 'closh-bool
                 :value (closh-pairp obj)))

(defun closh-list? (obj)
  (make-instance 'closh-bool
                 :value (closh-listp obj)))

(defun closh-symbol? (obj)
  (make-instance 'closh-bool
                 :value (closh-symbolp obj)))

(defun closh-boolean? (obj)
  (make-instance 'closh-bool
                 :value (closh-boolp obj)))

(defun closh-string? (obj)
  (make-instance 'closh-bool
                 :value (closh-strp obj)))

(defun closh-procedure? (obj)
  (make-instance 'closh-bool
                 :value (closh-procp obj)))
