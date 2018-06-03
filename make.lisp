;; $ sbcl --noinform --no-sysinit --no-userinit --load make.lisp
(require 'closh)
(in-package :closh)
(sb-ext:save-lisp-and-die "closh"
                          :toplevel #'closh-repl
                          :executable t)
