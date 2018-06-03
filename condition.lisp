(in-package :closh)

(defcondition closh-exit-signal (simple-condition))
(defcondition closh-error (simple-condition))

;;exec
(defcondition closh-unbound-error (closh-error) symname)
(defcondition closh-funcall-error (closh-error) callform)
(defcondition closh-type-error (closh-error))
(defcondition closh-argnum-error (closh-error) obj)
(defcondition closh-unquote-error (closh-error) form)

;; syntax check
(defcondition closh-malform-error (closh-error) spform)
(defcondition closh-call-error (closh-error) op)
(defcondition closh-syntax-error (closh-error) mes) ;;for other syntax error

;;conversion
(defcondition closh-from-scheme-error (closh-error) obj)
(defcondition closh-to-scheme-error (closh-error) obj)
(defcondition closh-cl-mode-error (closh-error))

(defmacro notice-error (mes-format &rest datum)
  `(progn
     (format t ,(concatenate 'string
                             "[closh-error] " mes-format "~%")
             ,@ datum)
     (force-output)))

(defgeneric handle-error (err))
(defmethod handle-error ((err closh-error))
  (notice-error "something wrong : ~a~%" (type-of err)))
(defmethod handle-error ((err closh-unbound-error))
  (notice-error "symbol is unbound : ~a" (symname err)))

(defmethod handle-error ((err closh-funcall-error))
  (notice-error "illeagal function call : ~a" (callform err)))

(defmethod handle-error ((err closh-type-error))
  (notice-error "argument type does not match"))

(defmethod handle-error ((err closh-argnum-error))
  (notice-error "invalid number of argument~%"))

(defmethod handle-error ((err closh-unquote-error))
  (notice-error "invalid unquoting : ~a"
                (dump-to-str (form err))))

(defmethod handle-error ((err closh-syntax-error))
  (notice-error "syntax error : ~a" (mes err)))

(defmethod handle-error ((err closh-malform-error))
  (notice-error "special-form call is malformed : ~a" (spform err)))

(defmethod handle-error ((err closh-call-error))
  (notice-error "operation call is not allowed here : ~a" (op err)))

(defmethod handle-error ((err closh-from-scheme-error))
  (notice-error "unsupported type of scheme object :~% ~a"
                (dump-to-str (obj err))))

(defmethod handle-error ((err closh-to-scheme-error))
  (notice-error "unsupported type of common lisp object :~% ~a" (obj err)))

(defmethod handle-error ((err closh-cl-mode-error))
  (notice-error "exec cl-mode failed"))

