(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new (insertR new old (cdr lat)))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cons old (insert new old (cdr lat)))))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst2
   (lambda (n o1 o2 lat)
    (cond
     ((null? lat) '())
     ((eq? o1 (car lat)) (cons n (cdr lat)))
     ((eq? o2 (car lat)) (cons n (cdr lat)))
     (else (cons (car lat) (subst2 n o1 o2 (cdr lat)))))))

(define multirember
  (lambda (x lat)
    (cond
     ((null? lat) '())
     ((eq? x (car lat)) (multirember x (cdr lat)))
     (else (cons (car lat) (multirember x (cdr lat)))))))
