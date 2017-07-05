(in-package :closh)

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body)) #'self))

(defmacro if-not (test-form else-form then-form)
  `(if ,test-form ,then-form ,else-form))

(defmacro define-class (class-name parent  &rest res)
  `(defclass ,class-name ,parent
     ,(mapcar (lambda (lis)
		(if (listp lis)
		    (apply(lambda (x &optional (y nil) (z x))
			    `(,x :initarg 
				 ,(intern (symbol-name x) "KEYWORD") 
				 :initform ,y :accessor ,z))
			  lis)
		    ((lambda (x) 
		       `(,x :initarg 
			    ,(intern (symbol-name x) "KEYWORD") 
			    :initform nil :accessor ,x))
		     lis)))
	      res)))

(defmacro defcondition (class-name parent &rest slots)
  `(define-condition ,class-name ,parent
     ,(loop for slot in slots collect
           (cond ((not (listp slot))
                  `(,slot :accessor ,slot
                          :initarg ,(intern (symbol-name slot)
                                            :keyword)))
                 ((= (length slot) 1)  
                  `(,(first slot) :accessor ,(first slot)
                                  :initarg ,(intern (symbol-name (first slot))
                                                    :keyword)))
                 ((= (length slot) 2)  
                  `(,(first slot) :accessor ,(first slot)
                                  :initarg ,(intern (symbol-name (first slot))
                                                    :keyword)
                                  :initform ,(second slot)))
                 ((= (length slot) 3)  
                  `(,(first slot) :accessor ,(third slot)
                                  :initarg ,(intern (symbol-name (first slot))
                                                    :keyword)
                                  :initform ,(second slot)))))))

