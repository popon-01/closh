(define hoge 1)

(define fuga '(1 2 3))

(define (fib1 n)
  (let loop ((a 0) (b 1) (cnt n))
    (if (= cnt 0) a (loop b (+ a b) (- cnt 1)))))

(define (fib2 n)
  (letrec ((loop
            (lambda (a b n)
              (if (= n 0) a
                  (loop b (+ a b) (- n 1))))))
    (loop 0 1 n)))

(define (fib3 n)
  (do ((a 0 b) (b 1 (+ a b)) (cnt n (- cnt 1)))
      ((= cnt 0) a)
    (write cnt)))

(define (fib-fool x)
  (cond ((= x 0) 0)
        ((= x 1) 1)
        (else (+ (fib-fool (- x 1))
                 (fib-fool (- x 2))))))

(define (make-add n)
  (let ((add n))
    (lambda (x) (+ x n))))

(defmacro (when test . body)
  `(if ,test (begin ,@body)))

(defmacro (aif test then else)
  `(let ((it ,test))
     (if it ,then ,else)))
