(in-package :closh)

(define-class closh-object ()
  (closh-boolp nil)
  (closh-macrop nil)
  (closh-symbolp nil)
  (closh-null nil))

;;object/exp
(define-class closh-exp (closh-object))

;;object/exp/sym
(define-class closh-sym (closh-exp) (closh-symbolp t) sym)

;;object/exp/const
(define-class closh-const (closh-exp) value)
(define-class closh-num (closh-const))
(define-class closh-bool (closh-const) (closh-boolp t))
(define-class closh-str (closh-const))

;;object/exp/list
(define-class closh-list (closh-exp))
(define-class closh-nil (closh-list)
  (closh-null t) (value nil))
(define-class closh-cons (closh-list) closh-car closh-cdr)

;;object/closh-op
(define-class closh-op (closh-object) (name "unknown"))

;;object/closh-op/
(define-class closh-func (closh-op)
  penv args body)
(define-class closh-macro (closh-op)
  penv args body (closh-macrop t))
(define-class closh-builtin (closh-op) func)

;;object/closh-op/special
(define-class closh-special (closh-op))
(define-class closh-define (closh-special) (name "define"))
(define-class closh-quote (closh-special) (name "quote"))
(define-class closh-set! (closh-special) (name "set!"))
(define-class closh-lambda (closh-special) (name "lambda"))
(define-class closh-let (closh-special) (name "let"))
(define-class closh-let* (closh-special) (name "let*"))
(define-class closh-letrec (closh-special) (name "letrec"))
(define-class closh-if (closh-special) (name "if"))
(define-class closh-cond (closh-special) (name "cond"))
(define-class closh-and (closh-special) (name "and"))
(define-class closh-or (closh-special) (name "or"))
(define-class closh-begin (closh-special) (name "begin"))

;; for object
;; convert to string
(defgeneric dump-to-str (obj))

;;for object
;;object represents true?
(defgeneric to-bool (obj))

;; for object
;; evaluate object in env
(defgeneric eval-closh-object (obj env))

;; for object/closh-op
;; call operation with argv(S-exp) in env
(defgeneric call-op (op argv env))


;;///// base-impl /////
(defmethod dump-to-str ((obj closh-object))
  (format nil "#<~a>" (type-of obj)))

(defmethod call-op ((obj closh-object) argv env)
  (declare (ignore argv env))
  (error "illeagal function call : ~a" (dump-to-str obj)))


;;///// dump-to-str /////
;;object/exp/sym
(defmethod dump-to-str ((sym closh-sym))
  (format nil "~a" (sym sym)))

;;object/exp/list
(defmethod dump-to-str ((cnil closh-nil))
  (format nil "()"))
(defmethod dump-to-str ((lst closh-cons))
  (let ((car-str (dump-to-str (closh-car lst)))
        (cdr-str (dump-to-str (closh-cdr lst))))
    (cond ((scan-to-strings "^\\(\\)$" cdr-str)
           (format nil "(~a)" car-str))
          ((scan-to-strings "^\\(.*\\)$" cdr-str)
           (format nil "(~a ~a)"
                   car-str (subseq cdr-str 1 (1- (length cdr-str)))))
          (t (format nil "(~a . ~a)" car-str cdr-str)))))

;;object/exp/const
(defmethod dump-to-str ((num closh-num))
  (format nil "~a" (value num)))
(defmethod dump-to-str ((bool closh-bool))
  (if (value bool) (format nil "#t") (format nil "#f")))
(defmethod dump-to-str ((str closh-str))
  (format nil "\"~a\"" (value str)))

;;///// to-bool /////
(defmethod to-bool ((obj closh-object)) t)
(defmethod to-bool ((b closh-bool)) (value b))




