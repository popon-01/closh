(in-package :closh)

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output)
  (read-line *standard-input*))

(defun read-eval-print-loop ()
  (block outer-repl
    (let ((line (raw-input "USER> ")))
      (block inner-repl
        (handler-bind ((closh-exit-signal
                        (lambda (c) (declare (ignore c))
                                (format t "exit.~%") (force-output)
                                (return-from outer-repl)))
                       (yacc-parse-error
                        (lambda (c) (declare (ignore c))
                                (format t "[closh-error] parse failed : ~a~%"
                                        line)
                                (force-output)
                                (return-from inner-repl)))
                       (closh-error
                        (lambda (c) (handle-error c)
                                (return-from inner-repl))))
          (closh-print
           (closh-eval-seq
            (closh-map #'check-toplevel
                       (closh-read line))
            *global-enviroment*)))))
    (read-eval-print-loop)))
  
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
              do (make-instance 'closh-do)))


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

(defun closh-repl ()
  (init-closh)
  (read-eval-print-loop))

