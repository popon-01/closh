(in-package :closh)

(defmacro malform-error (form)
  `(error 'closh-malform-error :spform ,form))

(defmacro syntax-assert (form &body preds)
  `(unless (and ,@preds)
     (malform-error ,form)))

(defun check-toplevel (exp)
  (let ((raw-exp (closh-macroexpand exp)))
    (cond ((not (and (closh-pairp raw-exp)
                     (closh-symbolp (closh-car raw-exp))))
           (check-exp raw-exp))
          ((eq (sym (closh-car raw-exp)) :load)
           (when (and (closh-listp exp)
                      (not (closh-nil-terminate-p exp)))
             (error 'closh-syntax-error
                    :mes "invalid list input")))
          ((some (lambda (x) (eq (sym (closh-car raw-exp)) x))
                 '(:define :defmacro))
           (check-define raw-exp))
          (t (check-exp raw-exp))))
  exp)

(defun check-define (exp)
  (when (and (closh-listp exp)
             (not (closh-nil-terminate-p exp)))
    (error 'closh-syntax-error
           :mes "invalid list input"))
  (cond ((and (eq (sym (closh-nth 0 exp)) :define)
              (closh-symbolp (closh-nth 1 exp)))
         (syntax-assert (sym (closh-car exp))
           (= (closh-length exp) 3))
         (check-exp (closh-macroexpand (closh-nth 2 exp))))
        ((closh-pairp (closh-nth 1 exp))
         (syntax-assert (sym (closh-car exp))
           (>= (closh-length exp) 2)
           (closh-symbolp (closh-car (closh-nth 1 exp))))
         (check-arg (closh-cdr (closh-nth 1 exp)))
         (check-body (closh-nthcdr 2 exp)))
        (t (malform-error (sym (closh-car exp))))))

(defun check-exp (exp)
  (when (and (closh-listp exp)
             (not (closh-nil-terminate-p exp)))
    (error 'closh-syntax-error
           :mes "invalid list input"))
  (when (closh-pairp exp)
    (if-not
     (closh-symbolp (closh-car exp))
     (closh-for-each
      #'check-exp
      (closh-map #'closh-macroexpand exp))
     (case (sym (closh-car exp))
       (:lambda (check-lambda (closh-cdr exp)))
       (:quote (check-quote (closh-cdr exp)))
       (:set! (check-set! (closh-cdr exp)))
       (:let (cond ((< (closh-length exp) 2)
                    (error 'closh-malform-error :spform :let))
                   ((closh-symbolp (closh-nth 1 exp))
                    (check-let (closh-nthcdr 2 exp) :let))
                   (t (check-let (closh-cdr exp) :let))))
       ((:let* :letrec)
        (check-let (closh-cdr exp) (sym (closh-car exp))))
       (:if (check-if (closh-cdr exp)))
       (:cond (check-cond (closh-cdr exp)))
       ((:and :or :begin)
        (closh-for-each
         #'check-exp
         (closh-macroexpand (closh-cdr exp))))
       (:do (check-do (closh-cdr exp)))
       (:clmode #|don't check|# )
       ((:define :defmacro :load)
        (error 'closh-call-error :op (sym (closh-car exp))))
       (t (closh-for-each
           #'check-exp
           (closh-map #'closh-macroexpand exp)))))))

(defun check-lambda (argv)
  (syntax-assert :lambda
    (>= (closh-length argv) 1))
  (check-arg (closh-nth 0 argv))
  (check-body (closh-nthcdr 1 argv)))

;; don't need "check-s-exp"
;; parser accept only s-exp
(defun check-quote (argv)
  (syntax-assert :quote
    (= (closh-length argv) 1)))

(defun check-set! (argv)
  (syntax-assert :set!
    (= (closh-length argv) 2)
    (closh-symbolp (closh-nth 0 argv)))
  (check-exp (closh-macroexpand (closh-nth 1 argv))))

(defun check-let (argv form)
  (syntax-assert form
    (>= (closh-length argv) 1)
    (closh-listp (closh-car argv)))
  ;;check Bindings
  (funcall
   (alambda (binds)
     (unless (closh-null binds)
       (let ((bind (closh-car binds)))
         (syntax-assert form
           (closh-listp bind)
           (closh-nil-terminate-p bind)
           (= (closh-length bind) 2)
           (closh-symbolp (closh-nth 0 bind)))
         (check-exp (closh-macroexpand (closh-nth 1 bind)))
         (self (closh-cdr binds)))))
   (closh-car argv))
  (check-body (closh-cdr argv)))

(defun check-if (argv)
  (syntax-assert :if
    (<= 2 (closh-length argv) 3))
  (closh-for-each
   #'check-exp
   (closh-map #'closh-macroexpand argv)))

(defun check-cond (argv)
  (unless (closh-null argv)
    (let ((clause (closh-car argv)))
      (syntax-assert :cond
        (closh-listp clause)
        (closh-nil-terminate-p clause)
        (>= (closh-length clause) 2))
      (if (and (closh-symbolp (closh-car clause))
               (eq (sym (closh-car clause)) :else))
          (progn
            (syntax-assert :cond
              (closh-null (closh-cdr argv)))
            (closh-for-each
             #'check-exp
             (closh-map #'closh-macroexpand (closh-cdr clause))))
          (progn
            (closh-for-each
             #'check-exp
             (closh-map #'closh-macroexpand clause))
            (check-cond (closh-cdr argv)))))))

(defun check-do (argv)
  (syntax-assert :do
    (>= (closh-length argv) 2))
  ;;check symbol binding block
  (funcall
   (alambda (binds)
     (unless (closh-null binds)
       (let ((bind (closh-car binds)))
         (syntax-assert :do
           (closh-listp bind)
           (closh-nil-terminate-p bind)
           (= (closh-length bind) 3)
           (closh-symbolp (closh-car bind)))
         (closh-for-each
          #'check-exp
          (closh-map #'closh-macroexpand (closh-cdr bind)))
         (self (closh-cdr binds)))))
   (closh-nth 0 argv))
  ;;check return value block
  (syntax-assert :do
    (closh-listp (closh-nth 1 argv))
    (closh-nil-terminate-p (closh-nth 1 argv))
    (>= (closh-length (closh-nth 1 argv)) 1))
  (closh-for-each
   #'check-exp
   (closh-map #'closh-macroexpand (closh-nth 1 argv)))
  ;;check body
  (check-body (closh-nthcdr 2 argv)))

(defun check-body (body)
  (funcall
   (alambda (body)
     (when (closh-null body) ;; body -> Define*
       (error 'closh-syntax-error
              :mes "No expression found in body"))
     (let ((elem (closh-car body)))
       (if (and (closh-pairp elem)
                (closh-symbolp (closh-car elem))
                (some (lambda (x) (eq (sym (closh-car elem)) x))
                      '(:define :defmacro)))
           (progn (check-define elem) (self (closh-cdr body)))
           (closh-for-each #'check-exp body)))) ;; rest body -> Exp+
   (closh-map #'closh-macroexpand body)))

(defun check-arg (arg)
  (cond ((or (closh-symbolp arg) (closh-null arg))) ;;do not check
        ((closh-pairp arg)
         (unless (closh-symbolp (closh-car arg))
           (error 'closh-syntax-error
                  :mes "malformed argument block"))
         (check-arg (closh-cdr arg)))
        (t (error 'closh-syntax-error
                  :mes "malformed argument block"))))
