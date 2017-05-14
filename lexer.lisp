(in-package :closh)

(define-string-lexer closh-lexer
  ("\\s")
  ("\\(" (return (values :lparen $@)))
  ("'\\(" (return (values :quote-lparen $@)))
  ("\\)" (return (values :rparen $@)))
  ("\\." (return (values :period $@)))
  ("#f" (return (values :false $@)))
  ("#t" (return (values :true $@)))
  ("[0-9A-Za-z!$%&*+-./<=>?@^_]+"
   (if (string= (scan-to-strings
                 "-?(0|[1-9][0-9]*(\\.[0-9]*)?)" $@) $@)
       (return (values :num $@))
       (return (values :sym $@))))
  ("." (return (values :undefined $@))))

(defun lexer-test (str)
  (funcall (alambda (lex)
             (multiple-value-bind (tag value) (funcall lex)
               (if tag (cons (cons tag value) (self lex)) nil)))
           (closh-lexer str)))
