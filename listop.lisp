(in-package :closh)

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

