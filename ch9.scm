(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 '()))))

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define a-pair?
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l)) #f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (length* (first pora))
               (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (o+ (o* 2 (weight* (first pora)))
               (weight* (second pora)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (o/ n 2)))
       (else (C (add1 (o* n 3)))))))))

(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n)
              (A n (sub1 m)))))))

(define will-stop?
  (lambda (f)
    (cond
     ((f '()) #t)
     (else #f))))

(define eternity
  (lambda (x)
    (eternity x)))

(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))

(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((lambda (l)
                  (cond
                   ((null? l) 0)
                   (else (add1 (eternity (cdr l))))))
                (cdr l))))))

((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 eternity)

((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))


((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

((lambda (mk-length)
   (mk-length eternity))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (mk-length (cdr l))))))))


(((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))
 '(apple))


((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 ((mk-length mk-length) (cdr l))))))))


(((lambda (lam)
     (lam lam))
   (lambda (lam)
     (lambda (a s)
       (cond
        ((null? s) '())
        ((eq? a (car s))
         (cdr s))
        (else (cons (car s) ((lam lam) a (cdr s))))))))
 'a '(1 2 3 a 4 5))


(((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
      (lambda (x) 
        ((mk-length mk-length) x)))))
 '(a))


((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x)
             ((f f) x)))))))

((Y (lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (length (cdr l))))))))
 '(a b))

