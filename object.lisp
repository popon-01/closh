(in-package :closh)

(define-class closh-object ()
  (closh-numberp nil)
  (closh-pairp nil)
  (closh-null nil)
  (closh-listp nil)
  (closh-symbolp nil)
  (closh-boolp nil)
  (closh-strp nil)
  (closh-procp nil)
  (closh-macrop nil))

;;object/exp
(define-class closh-exp (closh-object))

;;object/exp/sym
(define-class closh-sym (closh-exp) (closh-symbolp t) sym)

;;object/exp/const
(define-class closh-const (closh-exp) value)
(define-class closh-num (closh-const) (closh-numberp t))
(define-class closh-bool (closh-const) (closh-boolp t))
(define-class closh-str (closh-const) (closh-strp t))

;;object/exp/list
(define-class closh-list (closh-exp) (closh-listp t))
(define-class closh-nil (closh-list)
  (closh-null t) (value nil))
(define-class closh-cons (closh-list)
  (closh-pairp t) closh-car closh-cdr)

;;object/closh-op
(define-class closh-op (closh-object) (name "unknown"))

;;object/closh-op/procedure
(define-class closh-procedure (closh-op) (closh-procp t))
(define-class closh-func (closh-procedure)
  penv args body)
(define-class closh-macro (closh-procedure)
  penv args body (closh-macrop t))
(define-class closh-builtin (closh-procedure) func)

;;object/closh-op/special
(define-class closh-special (closh-op))
(define-class closh-define (closh-special) (name "define"))
(define-class closh-defmacro (closh-special) (name "defmacro"))
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
(define-class closh-do (closh-special) (name "do"))

;;object
(define-class closh-undef (closh-object))

;; for object
;; convert to string
(defgeneric dump-to-str (obj))

;;for object
;;object represents true?
(defgeneric to-bool (obj))

;; for object
;; evaluate object in env
(defgeneric closh-eval-object (obj env))

;; for closh-list
;; evaluate each elements in lst in env
(defgeneric closh-eval-all (lst env))

;; for closh-list
;; evaluate elements in lst sequently
(defgeneric closh-eval-seq (lst env))

;; for object/closh-op
;; call operation with argv(S-exp) in env
(defgeneric call-op (op argv env))

;; for object/exp
;; macroexpand functions
(defgeneric macro-callp (obj env))
(defgeneric closh-macroexpand-1 (exp env))
(defgeneric closh-macroexpand (exp env))

;;///// base-impl /////
(defmethod dump-to-str ((obj closh-object))
  (format nil "#<~a>" (type-of obj)))

(defmethod call-op ((obj closh-object) argv env)
  (declare (ignore argv env))
  (error 'closh-funcall-error :callform (dump-to-str obj)))


;;///// dump-to-str /////
;;object/exp/sym
(defmethod dump-to-str ((sym closh-sym))
  (format nil "~a" (sym sym)))

;;object/exp/list
(defmethod dump-to-str ((lst closh-nil))
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




