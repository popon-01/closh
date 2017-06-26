(in-package :closh)

(define-condition closh-exit-signal (simple-condition) ())
(define-condition closh-error (simple-condition) ())
(define-condition closh-unbound-error (closh-error)
  ((symname :accessor symname :initarg :symname)))
(define-condition closh-funcall-error (closh-error)
  ((callform :accessor callform :initarg :callform)))
(define-condition closh-type-error (closh-error)
  ())


(defgeneric handle-error (err))
(defmethod handle-error ((err closh-unbound-error))
  (format t "[closh-error] symbol is unbound : ~a~%"
          (symname err))
  (force-output))

(defmethod handle-error ((err closh-funcall-error))
  (format t "[closh-error] illeagal function call : ~a~%"
          (callform err))
  (force-output))

(defmethod handle-error ((err closh-type-error))
  (format t "[closh-errror] argument type does not match~%")
  (force-output))



