(in-package :closh)

(defcondition closh-exit-signal (simple-condition))
(defcondition closh-error (simple-condition))

;;exec
(defcondition closh-unbound-error (closh-error) symname)
(defcondition closh-funcall-error (closh-error) callform)
(defcondition closh-type-error (closh-error))
(defcondition closh-argnum-error (closh-error) obj)

;; syntax check
(defcondition closh-malform-error (closh-error) spform)
(defcondition closh-call-error (closh-error) op)
(defcondition closh-syntax-error (closh-error) mes) ;;for other syntax error

;;conversion
(defcondition closh-from-scheme-error (closh-error) obj)
(defcondition closh-to-scheme-error (closh-error) obj)
(defcondition closh-cl-mode-error (closh-error))

(defgeneric handle-error (err))
(defmethod handle-error ((err closh-error))
  (format t "[closh-error] something wrong : ~a~%" (type-of err))
  (force-output))

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

(defmethod handle-error ((err closh-argnum-error))
  (format t "[closh-error] invalid number of argument~%")
  (force-output))

(defmethod handle-error ((err closh-syntax-error))
  (format t "[closh-errror] syntax error: ~a~%"
          (mes err))
  (force-output))

(defmethod handle-error ((err closh-malform-error))
  (format t "[closh-errror] special-form call is malformed :~a~%"
          (spform err))
  (force-output))

(defmethod handle-error ((err closh-call-error))
  (format t "[closh-errror] operation call is not allowed here : ~a~%"
          (op err))
  (force-output))

(defmethod handle-error ((err closh-from-scheme-error))
  (format t "[closh-error] unsupported type of scheme object :~% ~a~%"
          (dump-to-str (obj err)))
  (force-output))

(defmethod handle-error ((err closh-to-scheme-error))
  (format t "[closh-error] unsupported type of common lisp object :~% ~a~%"
          (obj err))
  (force-output))

(defmethod handle-error ((err closh-cl-mode-error))
  (format t "[closh-error] exec cl-mode failed~%")
  (force-output))

