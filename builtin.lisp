(in-package :closh)

(defmethod call-op ((op closh-builtin) (argv closh-list)
                    (env closh-env))
  (apply (func op) (revert-closh-list
                    (closh-map (lambda (exp) (eval-closh-object exp env))
                               argv))))

(defun closh-exit (&rest argv)
  (declare (ignore argv))
  (make-instance 'exit-signal))
