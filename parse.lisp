(in-package :closh)

(define-string-lexer closh-lexer
  ("\\s")
  ("\\("    (return (values 'lparen 'lparen)))
  ("\\)"    (return (values 'rparen 'rparen)))
  ("\\."    (return (values 'period 'period)))
  ("'"      (return (values 'quote 'quote)))
  ("#f"     (return (values 'bool 'false)))
  ("#t"     (return (values 'bool 'true)))
  ("\"([^\\\"]|\\.)*?\"" (return (values 'string (string-trim "\"" $@))))
  ("[0-9A-Za-z!$%&*+-./<=>?@^_]+"
   (if (string= (scan-to-strings
                 "-?(0|[1-9][0-9]*(\\.[0-9]*)?)" $@) $@)
       (return (values 'num (read-from-string $@)))
       (return (values 'sym (intern $@ 'keyword)))))
  ("." (return (values 'undefined 'undefined))))

(define-parser closh-parser
  (:start-symbol closh-transration-unit)
  (:terminals (lparen rparen period quote
               bool string sym num))
  (closh-transration-unit (exp closh-transration-unit
                           (lambda (exp unit) (cons exp unit)))
                          (exp
                           (lambda (exp) (list exp))))
  (exp (num
        (lambda (num) (make-instance 'closh-const :value num)))
       (bool
        (lambda (bool) (make-instance 'closh-const :value bool)))
       (string
        (lambda (str) (make-instance 'closh-const :value str)))
       (lparen rparen
        (lambda (lp rp) (declare (ignore lp rp)) nil))
       (sym
        (lambda (sym) (make-instance 'closh-sym :sym sym)))
       (lparen multiple-exp rparen
        (lambda (lp exps rp) (declare (ignore lp rp)) exps))
       (quote exp
        (lambda (q exp) (declare (ignore q))
                (list (make-instance 'closh-sym :sym :quote)
                      exp))))

  (multiple-exp (exp multiple-exp
                 (lambda (exp exps) (cons exp exps)))                
                (exp
                 (lambda (exp) (list exp)))
                (exp period exp
                 (lambda (exp1 exp2) (cons exp1 exp2)))))

(defun lex-test (str)
  (funcall (alambda (lex)
             (multiple-value-bind (tag value) (funcall lex)
               (if tag (cons (cons tag value) (self lex)) nil)))
           (closh-lexer str)))

(defun dump-parse-result (res)
  (if (listp res)
      (mapcar #'dump-parse-result res)
      (dump-to-str res)))

(defun parse-test (str)
  (dump-parse-result
   (parse-with-lexer (closh-lexer str) closh-parser)))

