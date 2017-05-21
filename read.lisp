(in-package :closh)

(define-string-lexer closh-lexer
  ("\\s")
  ("\\("    (return (values 'lparen 'lparen)))
  ("\\)"    (return (values 'rparen 'rparen)))
  ("\\."    (return (values 'period 'period)))
  ("'"      (return (values 'quote 'quote)))
  ("#f"     (return (values 'bool nil)))
  ("#t"     (return (values 'bool t)))
  ("\"([^\\\"]|\\.)*?\"" (return (values 'string (string-trim "\"" $@))))
  ("[0-9A-Za-z!$%&*+-./<=>?@^_]+"
   (if (string= (scan-to-strings
                 "-?(0|[1-9][0-9]*(\\.[0-9]*)?)" $@) $@)
       (return (values 'num (read-from-string $@)))
       (return (values 'sym (intern $@ 'keyword)))))
  ("." (return (values 'undefined 'undefined))))

(define-parser closh-parser
  (:start-symbol closh)
  (:terminals (lparen rparen period quote
                      bool string sym num))

  (closh (exp closh (lambda (exp closh)
                      (cons exp closh)))
         (exp (lambda (exp) (list exp))))

  (exp (num (lambda (num)
              (make-instance 'closh-num :value num)))
       (bool (lambda (bool)
               (make-instance 'closh-bool :value bool)))
       (string (lambda (str)
                 (make-instance 'closh-str :value str)))
       (lparen rparen (lambda (lp rp)
                        (declare (ignore lp rp))
                        (make-closh-list)))
       (sym (lambda (sym)
              (make-instance 'closh-sym :sym sym)))
       (lparen multiple-exp rparen (lambda (lp exps rp)
                                     (declare (ignore lp rp)) exps))
       (quote exp (lambda (q exp)
                    (declare (ignore q))
                    (make-closh-list (make-instance 'closh-sym
                                                    :sym :quote)
                                     exp))))
  (multiple-exp (exp multiple-exp (lambda (exp exps)
                                    (make-closh-cons exp exps))) 
                (exp (lambda (exp)
                       (make-closh-list exp)))
                (exp period exp (lambda (exp1 exp2)
                                  (make-closh-cons exp1 exp2)))))

(defun lex-test (str)
  (funcall (alambda (lex)
             (multiple-value-bind (tag value) (funcall lex)
               (if tag (cons (cons tag value) (self lex)) nil)))
           (closh-lexer str)))

(defun parse-test (str)
  (parse-with-lexer (closh-lexer str) closh-parser))

(defun dump-parse-test (str)
  (mapcar #'dump-to-str (parse-test str)))

