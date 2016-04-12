(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? n) m)
     (else (o+ (sub1 n) (add1 m))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o- (sub1 n) (sub1 m))))))

