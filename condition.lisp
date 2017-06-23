(in-package :closh)

(define-condition closh-exit-signal (simple-condition) ())
(define-condition closh-error (simple-condition) ())
(define-condition closh-unbound-error (closh-error)
  ((symname :accessor symname :initarg :symname)))


(defgeneric handle-error (err))

(defmethod handle-error ((err closh-unbound-error))
  (format t "closh-error: symbol ~a is unbound.~%" (symname err))
  (force-output))



