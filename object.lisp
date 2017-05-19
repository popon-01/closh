(in-package :closh)

(define-class closh-object ())

(define-class closh-exp (closh-object))

(define-class closh-list (closh-exp))
(define-class closh-nil (closh-list) (value nil))
(define-class closh-cons (closh-list) closh-car closh-cdr)

(define-class closh-sym (closh-exp) sym)

(define-class closh-const (closh-exp) value)
(define-class closh-num (closh-const))
(define-class closh-bool (closh-const))
(define-class closh-str (closh-const))

;;dump-to-str
;;convert closh-exp to string
(defgeneric dump-to-str (obj))

(defmethod dump-to-str ((obj closh-object))
  (class-name obj))

(defmethod dump-to-str ((lst closh-list))
  (format nil "(~a . ~a)"
          (dump-to-str (closh-car lst))
          (dump-to-str (closh-cdr lst))))

(defmethod dump-to-str ((const closh-const))
  (format nil "[const : ~a]" (value const)))

(defmethod dump-to-str ((sym closh-sym))
  (format nil "[symbol : ~a]" (sym sym)))


;;make-closh-cons, make-closh-list
;;easy syntax to make closh-cons
(defgeneric make-closh-cons (car-value cdr-value))
(defmethod make-closh-cons ((car-value closh-exp)
                            (cdr-value closh-exp))
  (make-instance 'closh-cons
                 :closh-car car-value
                 :closh-cdr cdr-value))

(defun make-closh-list (&rest vals)
  (if (null vals)
      (make-instance 'closh-nil)
      (make-instance 'closh-cons
                     :closh-car (car vals)
                     :closh-cdr (make-closh-list (cdr vals)))))

;;closh-null
;;function "null" for closh expression
(defgeneric closh-null (obj))
(defmethod closh-null ((obj closh-exp)) nil)
(defmethod closh-null ((obj closh-nil)) t)



