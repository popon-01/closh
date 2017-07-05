(in-package :closh)

(defvar *global-enviroment*)

(define-class closh-env () (table (make-hash-table)))
(define-class closh-global (closh-env))
(define-class closh-local (closh-env) parent)

(defgeneric add-env (sym value env))
(defgeneric get-env (sym env))
(defgeneric update-env (sym value env))

(defmethod add-env ((sym closh-sym) (value closh-object)
                    (env closh-env))
  (setf (gethash (sym sym) (table env)) value) env)

;; for init
(defmethod add-env ((sym symbol) (value closh-object)
                    (env closh-env))
  (setf (gethash sym (table env)) value) env)


(defmethod get-env ((sym closh-sym) (env closh-global))
  (multiple-value-bind (val foundp) (gethash (sym sym) (table env))
    (if foundp val (error 'closh-unbound-error
                          :symname (symbol-name (sym sym))))))

(defmethod get-env ((sym closh-sym) (env closh-local))
  (multiple-value-bind (val foundp) (gethash (sym sym) (table env))
    (if foundp val (get-env sym (parent env)))))


(defmethod update-env ((sym closh-sym) (value closh-object)
                         (env closh-global))
  (multiple-value-bind (old foundp) (gethash (sym sym) (table env))
    (declare (ignore old))
    (if foundp
        (progn (setf (gethash (sym sym) (table env)) value) env)
        (error 'closh-unbound-error :symname (symbol-name (sym sym))))))

(defmethod update-env ((sym closh-sym) (value closh-object)
                         (env closh-local))
  (multiple-value-bind (old foundp) (gethash (sym sym) (table env))
    (declare (ignore old))
    (if foundp
        (progn (setf (gethash (sym sym) (table env)) value) env)
        (update-env sym value (parent env)))))

(defmacro add-global (&body key-and-values)
  `(progn
     ,@(iterate (for l on key-and-values by #'cddr)
                (collect
                    `(add-env ,(intern (symbol-name (first l))
                                       :keyword)
                              ,(second l)
                              *global-enviroment*)))))

(defmacro add-global-builtin (&body key-and-funcs)
  `(add-global
     ,@(iterate (for l on key-and-funcs by #'cddr)
                (nconcing
                 (list (first l)
                       `(make-instance 'closh-builtin
                                       :func ,(second l)))))))

(defun add-special ()
  (add-global define (make-instance 'closh-define)
              defmacro (make-instance 'closh-defmacro)
              quote (make-instance 'closh-quote)
              set! (make-instance 'closh-set!)
              lambda (make-instance 'closh-lambda)
              let (make-instance 'closh-let)
              let* (make-instance 'closh-let*)
              letrec (make-instance 'closh-letrec)
              if (make-instance 'closh-if)
              cond (make-instance 'closh-cond)
              or (make-instance 'closh-or)
              and (make-instance 'closh-and)
              begin (make-instance 'closh-begin)
              do (make-instance 'closh-do)
              cl-mode (make-instance 'closh-cl-mode)))


(defun add-number-func ()
  (add-global-builtin number? (closh-type-pred #'closh-numberp)
                      + (closh-num-calc #'+)
                      - (closh-num-calc #'-)
                      * (closh-num-calc #'*)
                      / (closh-num-calc #'/)
                      = (closh-num-pred #'=)
                      < (closh-num-pred #'<)
                      <= (closh-num-pred #'<=)
                      > (closh-num-pred #'>)
                      >= (closh-num-pred #'>=)))

(defun add-list-func ()
  (add-global-builtin null? (closh-type-pred #'closh-null)
                      pair? (closh-type-pred #'closh-pairp)
                      list? (closh-type-pred #'closh-listp)
                      symbol? (closh-type-pred #'closh-symbolp)
                      car #'closh-car
                      cdr #'closh-cdr
                      cons #'make-closh-cons
                      list #'make-closh-list
                      length (lambda (arg)
                               (make-instance 'closh-num
                                              :value (closh-length arg)))
                      memq #'closh-memq
                      last #'closh-last
                      append #'closh-append
                      set-car! #'closh-set-car!
                      set-cdr! #'closh-set-cdr!))

(defun add-bool-func ()
  (add-global-builtin boolean? (closh-type-pred #'closh-boolp)
                      not #'closh-not))

(defun add-str-func ()
  (add-global-builtin string? (closh-type-pred #'closh-strp)
                      string-append #'closh-string-append
                      symbol->string #'closh-symbol->string
                      string->symbol #'closh-string->symbol
                      number->string #'closh-number->string
                      string->number #'closh-string->number))

(defun add-comp-func ()
  (add-global-builtin eq? #'closh-eq?
                      neq? #'closh-neq?
                      equal? #'closh-equal?))

(defun add-other-func ()
  (add-global-builtin exit #'closh-exit
                      procedure? (closh-type-pred #'closh-procp)
                      read (lambda (str)
                             (closh-car (closh-read (value str))))
                      macroexpand #'closh-macroexpand
                      macroexpand-1 #'closh-macroexpand-1
                      load #'closh-load
                      write #'closh-print))

(defun init-closh ()
  (setf *global-enviroment* (make-instance 'closh-global))
  (add-special)
  (add-number-func)
  (add-list-func)
  (add-bool-func)
  (add-str-func)
  (add-comp-func)
  (add-other-func))
