(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? l) l)
     ((eq? a (car l)) (rember* a (cdr l)))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
