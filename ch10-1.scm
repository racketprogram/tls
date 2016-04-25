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

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else  (o+ n (o* n (sub1 m)))))))

(define o^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (o^ n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (o/ (o- n m) m))))))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
     ((or (o> n m) (o< n m)) #f)
     (else #t))))
     
