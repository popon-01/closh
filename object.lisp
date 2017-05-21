(in-package :closh)

(define-class closh-object ()
  (closh-macrop nil) (closh-symbolp nil))

(define-class closh-exp (closh-object))

(define-class closh-list (closh-exp))
(define-class closh-nil (closh-list) (value nil))
(define-class closh-cons (closh-list) closh-car closh-cdr)

(define-class closh-sym (closh-exp) (closh-symbolp t) sym)

(define-class closh-const (closh-exp) value)
(define-class closh-num (closh-const))
(define-class closh-bool (closh-const))
(define-class closh-str (closh-const))

;;dump-to-str
;;convert closh-exp to string
(defgeneric dump-to-str (obj))

(defmethod dump-to-str ((obj closh-object))
  (class-name obj))

(defmethod dump-to-str ((lst closh-cons))
  (format nil "(~a . ~a)"
          (dump-to-str (closh-car lst))
          (dump-to-str (closh-cdr lst))))

(defmethod dump-to-str ((cnil closh-nil))
  (format nil "()"))

(defmethod dump-to-str ((num closh-num))
  (format nil "~a" (value num)))
(defmethod dump-to-str ((bool closh-bool))
  (if (value bool) (format nil "#t") (format nil "#f")))
(defmethod dump-to-str ((str closh-str))
  (format nil "\"~a\"" (value str)))

(defmethod dump-to-str ((sym closh-sym))
  (format nil "~a" (symbol-name (sym sym))))


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
                     :closh-cdr (apply #'make-closh-list
                                       (cdr vals)))))

;;closh-null
;;function "null" for closh expression
(defgeneric closh-null (obj))
(defmethod closh-null ((obj closh-exp)) nil)
(defmethod closh-null ((obj closh-nil)) t)

(defgeneric closh-length (lst))
(defmethod closh-length ((lst closh-list))
  (if (closh-null lst) 0 (1+ (closh-length (closh-cdr lst)))))

(defgeneric closh-reverse (lst))
(defmethod closh-reverse ((lst closh-list))
  (funcall
   (alambda (lst acc)
     (if (closh-null lst)
         acc
         (self (closh-cdr lst)
               (make-closh-cons (closh-car lst) acc))))
   lst (make-instance 'closh-nil)))


(defun closh-append (&rest lsts)
  (funcall
   (alambda (lsts acc)
     (cond ((null lsts) (closh-reverse acc))
           ((closh-null (car lsts))
            (self (cdr lsts) acc))
           (t (self (cons (closh-cdr (car lsts)) (cdr lsts))
                    (make-closh-cons (closh-car (car lsts))
                                     acc)))))
   lsts (make-instance 'closh-nil)))

(defgeneric closh-map (func lst))
(defmethod closh-map (func (lst closh-list))
  (funcall
   (alambda (lst acc)
     (if (closh-null lst)
         (closh-reverse acc)
         (self (closh-cdr lst)
               (make-closh-cons (funcall func (closh-car lst))
                                acc))))
   lst (make-instance 'closh-nil)))




