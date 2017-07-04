(in-package :closh)

(define-condition closh-exit-signal (simple-condition) ())
(define-condition closh-error (simple-condition) ())

;;exec
(define-condition closh-unbound-error (closh-error)
  ((symname :accessor symname :initarg :symname)))
(define-condition closh-funcall-error (closh-error)
  ((callform :accessor callform :initarg :callform)))
(define-condition closh-type-error (closh-error)
  ())

;; syntax check
(define-condition closh-syntax-error (closh-error)
  ((mes :accessor mes :initarg :mes)))
(define-condition closh-malform-error (closh-error)
  ((spform :accessor spform :initarg :spform)))
(define-condition closh-call-error (closh-error)
  ((op :accessor op :initarg :op)))

;;compat
(define-condition closh-from-scheme-error (closh-error)
  ((obj :accessor obj :initarg :obj)))
(define-condition closh-to-scheme-error (closh-error)
  ((obj :accessor obj :initarg :obj)))

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
  (format t "[closh-error] unsupported type of scheme object : ~a~%"
          (dump-to-str (obj err)))
  (force-output))
(defmethod handle-error ((err closh-to-scheme-error))
  (format t "[closh-error] unsupported type of common lisp object : ~a~%"
          (obj err))
  (force-output))




