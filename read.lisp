(in-package :closh)

(define-string-lexer closh-lexer
  ("\\s")
  ("\\("    (return (values 'lparen 'lparen)))
  ("\\)"    (return (values 'rparen 'rparen)))
  ("\\."    (return (values 'period 'period)))
  ("'"      (return (values 'quote 'quote)))
  ("`"      (return (values 'quasiquote 'quasiquote)))
  (",@"     (return (values 'unquote-splicing 'unquote-splicing)))
  ("\\,"    (return (values 'unquote 'unquote)))
  ("#f"     (return (values 'bool nil)))
  ("#t"     (return (values 'bool t)))
  ("\"([^\\\\\"]|\\\\.)*?\"" (return (values 'string
                                             (subseq $@ 1 (1- (length $@))))))
  ("[0-9A-Za-z!$%&*+-./<=>?@^_]+"
   (if (scan-to-strings "^-?(0|[1-9][0-9]*(\\.[0-9]*)?)$" $@)
       (return (values 'num (read-from-string $@)))
       (return (values 'sym (intern (string-upcase $@) 'keyword)))))
  ("." (return (values 'undefined 'undefined))))


(define-parser closh-parser
  (:start-symbol closh)
  (:terminals (lparen rparen period
               quote quasiquote
               unquote unquote-splicing
               bool string sym num))
  
  (closh
   (exp closh
     (lambda (exp closh) (make-closh-cons exp closh)))
   (exp
    (lambda (exp) (make-closh-list exp))))
  
  (exp
   (atom
    (lambda (atom) atom))
   (lparen multiple-exp rparen
    (lambda (lp exps rp) (declare (ignore lp rp)) exps))
   (quote exp
    (lambda (q exp) (declare (ignore q))
            (make-closh-list (make-instance 'closh-sym :sym :quote)
                             exp)))
   (quasiquote exp
    (lambda (qq exp) (declare (ignore qq))
            (make-closh-list (make-instance 'closh-sym :sym :quasiquote)
                             exp)))
   (unquote exp
    (lambda (uq exp) (declare (ignore uq))
            (make-closh-list (make-instance 'closh-sym :sym :unquote)
                             exp)))
   (unquote-splicing exp
    (lambda (uq-splice exp) (declare (ignore uq-splice))
            (make-closh-list (make-instance 'closh-sym :sym :unquote-splicing)
                             exp))))
  
  (multiple-exp
   (exp multiple-exp
    (lambda (exp exps) (make-closh-cons exp exps))) 
   (exp
    (lambda (exp) (make-closh-list exp)))
   (exp period exp
    (lambda (exp1 p exp2) (declare (ignore p)) (make-closh-cons exp1 exp2))))
    
  (atom
   (num
    (lambda (num) (make-instance 'closh-num :value num)))
   (bool
    (lambda (bool) (make-instance 'closh-bool :value bool)))
   (string
    (lambda (str) (make-instance 'closh-str :value str)))
   (lparen rparen
    (lambda (lp rp) (declare (ignore lp rp)) (make-closh-list)))
   (sym
    (lambda (sym) (make-instance 'closh-sym :sym sym)))))

(defun closh-file-lexer (path)
  (let ((in (open path)) (clexer nil) (endp nil))
    (alambda ()
      (cond (endp nil)
            (clexer
             (multiple-value-bind (tag val) (funcall clexer)
               (if tag (values tag val)
                   (progn (setf clexer nil) (self)))))
            (t (aif (read-line in nil nil)
                    (progn (setf clexer (closh-lexer it)) (self))
                    (progn (setf endp t) (close in) (self))))))))

(defun closh-load (str)
  (closh-eval-seq (parse-with-lexer (closh-file-lexer (value str))
                                    closh-parser))
  str)

(defun closh-read (str)
  (parse-with-lexer (closh-lexer str) closh-parser))


(defun lex-test (str)
  (funcall (alambda (lex)
             (multiple-value-bind (tag value) (funcall lex)
               (if tag (cons (cons tag value) (self lex)) nil)))
           (closh-lexer str)))

(defun parse-test (str)
  (parse-with-lexer (closh-lexer str) closh-parser))

(defun dump-parse-test (str)
  (mapcar #'dump-to-str (parse-test str)))

