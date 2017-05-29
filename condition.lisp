(in-package :closh)

(define-condition closh-condition (simple-condition) ())
(define-condition closh-exit-signal (closh-condition) ())

