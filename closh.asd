(asdf:defsystem :closh
  :description "A Mini-Scheme Implementation in Common Lisp"
  :author "Arimichi MATSUMURA <arimichi@psg.c.titech.ac.jp>"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "object")
               (:file "listop")
               (:file "env")
               (:file "func")
               (:file "special")
               (:file "builtin")
               (:file "read")
               (:file "eval")
               (:file "closh"))
  :depends-on (:alexandria :iterate :anaphora :split-sequence
               :cl-ppcre :cl-lex :yacc))

