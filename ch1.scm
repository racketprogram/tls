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

(define insert
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons (car lat) (cons new (insert new old (cdr lat)))))
     (else (cons (car lat) (insert new old (cdr lat)))))))
(define subset
   (lambda (n o1 o2 lat)
    (cond
     ((null? lat) '())
     ((eq? o1 (car lat)) (cons n (cdr lat)))
     ((eq? o2 (car lat)) (cons n (cdr lat)))
     (else (cons (car lat) (subset n o1 o2 (cdr lat)))))))
(defne multirember
  (lambda (x lat)
    (cond
     ((null? lat) '())
     ((eq? x (car lat)) (multirember x (cdr lat)))
     (else (cons (car lat) (multirember x (cdr lat)))))))
