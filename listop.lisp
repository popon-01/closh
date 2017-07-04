(in-package :closh)

(define-symbol-macro cnil
    (make-instance 'closh-nil))

(defgeneric make-closh-cons (car-value cdr-value))
(defmethod make-closh-cons ((car-value closh-object)
                            (cdr-value closh-object))
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


(defgeneric closh-memq (item lst))
(defmethod closh-memq ((item closh-object) (lst closh-nil))
  (make-instance 'closh-bool :value nil))
(defmethod closh-memq ((item closh-object) (lst closh-cons))
  (if (to-bool (closh-eq? item (closh-car lst))) lst
      (closh-memq item (closh-cdr lst))))

(defgeneric closh-last (lst))
(defmethod closh-last ((lst closh-cons))
  (if-not (closh-pairp (closh-cdr lst))
          (closh-car lst)
          (closh-last (closh-cdr lst))))

(defgeneric closh-lastcell (lst))
(defmethod closh-lastcell ((lst closh-nil)) lst)
(defmethod closh-lastcell ((lst closh-cons))
  (if-not (closh-pairp (closh-cdr lst))
          lst (closh-lastcell (closh-cdr lst))))

(defgeneric closh-nil-terminate-p (lst))
(defmethod closh-nil-terminate-p ((lst closh-nil)) t)
(defmethod closh-nil-terminate-p ((lst closh-cons))
  (closh-null (closh-cdr (closh-lastcell lst))))

(defgeneric closh-set-car! (lst val))
(defmethod closh-set-car! ((lst closh-cons) (val closh-object))
  (setf (closh-car lst) val) val)

(defgeneric closh-set-cdr! (lst val))
(defmethod closh-set-cdr! ((lst closh-cons) (val closh-object))
  (setf (closh-cdr lst) val) val)


(defgeneric closh-map (func lst))
(defmethod closh-map (func (lst closh-list))
  (funcall
   (alambda (lst acc)
     (if (closh-null lst)
         (closh-reverse acc)
         (self (closh-cdr lst)
               (make-closh-cons (funcall func (closh-car lst))
                                acc))))
   lst cnil))

(defgeneric closh-for-each (func lst))
(defmethod closh-for-each (func (lst closh-list))
  (funcall
   (alambda (lst)
     (if (closh-null lst) cnil
         (progn
           (funcall func (closh-car lst))
           (self (closh-cdr lst)))))
   lst))


(defgeneric closh-nth (n lst))
(defmethod closh-nth (n (lst closh-list))
  (if (zerop n)
      (closh-car lst)
      (closh-nth (1- n) (closh-cdr lst))))
(defmethod closh-nth (n (lst closh-nil))
  lst)

(defgeneric closh-nth-all (n lists))
(defmethod closh-nth-all (n lists)
  (closh-map (lambda (lst) (closh-nth n lst))
             lists))

(defgeneric closh-nthcdr (n lst))
(defmethod closh-nthcdr (n (lst closh-list))
  (if (zerop n)
      lst (closh-nthcdr (1- n) (closh-cdr lst))))
(defmethod closh-nthcdr (n (lst closh-nil))
  lst)

(defgeneric unpack-closh-list (lst))
(defmethod unpack-closh-list ((lst closh-nil)) nil)
(defmethod unpack-closh-list ((lst closh-cons))
  (cons (closh-car lst)
        (unpack-closh-list (closh-cdr lst))))
