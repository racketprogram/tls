(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

(define add1
  (lambda (x)
    (o+ x 1)))

(lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (eternity (cdr l))))))

(define eternity
  (lambda (x)
    (eternity x)))

((lambda (i)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (i (cdr l)))))))
 (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l)))))))


((lambda (i)
   (i
    (i
     (i eternity))))
 (lambda (e)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (e (cdr l))))))))
 

((lambda (f)
   (f f))
 (lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l))))))))


((lambda (f)
     (f f))
   (lambda (f)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 ((f f) (cdr l))))))))

;;fail
((lambda (f)
   (f f))
 (lambda (f)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (f f))))


((lambda (f)
     (f f))
   (lambda (f)
     ((lambda (length)
        (lambda (l)
          (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
      (lambda (x)
        ((f f) x)))))

((lambda (le)
   ((lambda (f)
      (f f))
    (lambda (f)
      (le (lambda (x)
            ((f f) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

(lambda (le)
  ((lambda (f)
     (f f))
   (lambda (f)
     (le (lambda (x)
           ((f f) x))))))
