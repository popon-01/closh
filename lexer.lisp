(in-package :closh)

(define-string-lexer closh-lexer
  ("\\s")
  ("\\(" (return (values :lparen :lparen)))
  ("'\\(" (return (values :quote-lparen :quote-lparen)))
  ("\\)" (return (values :rparen :rparen)))
  ("\\." (return (values :period :period)))
  ("#f" (return (values :bool :false)))
  ("#t" (return (values :bool :true)))
  ("[0-9A-Za-z!$%&*+-./<=>?@^_]+"
   (if (string= (scan-to-strings
                 "-?(0|[1-9][0-9]*(\\.[0-9]*)?)" $@) $@)
       (return (values :num (read-from-string $@)))
       (return (values :sym (intern $@ :keyword)))))
  ("." (return (values :undefined :undefined))))

(defun lexer-test (str)
  (funcall (alambda (lex)
             (multiple-value-bind (tag value) (funcall lex)
               (if tag (cons (cons tag value) (self lex)) nil)))
           (closh-lexer str)))
