(in-package :closh)

(defmethod call-op ((op op-quote) (argv closh-cons) (env closh-env))
  (declare (ignore op env)) (closh-car argv))

