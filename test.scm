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

(define (make-add n)
  (let ((add n))
    (lambda (x) (+ x n))))

(define hoge 1)

(define (plus2 x) (+ x 2))

