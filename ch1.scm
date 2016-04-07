(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define first
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (first (cdr l)))))))
