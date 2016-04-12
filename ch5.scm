(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember**
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? l) l)
     ((eq? a (car l)) (rember** a (cdr l)))
     (else (cons (rember** a (car l)) (rember** a (cdr l)))))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) a) (rember* a (cdr l)))
                       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (n o l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) o) (cons o (cons n (insertR* n o (cdr l)))))
                       (else (cons (car l) (insertR* n o (cdr l))))))
     (else (cons (insertR* n o (car l)) (insertR* n o (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
                       ((eq? (car l) a) (add1 (occur* a (cdr l))))
                       (else (occur* a (cdr l)))))
     (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subset*
  (lambda (n o l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) o) (cons n (subset* n o (cdr l))))
                       (else (cons (car l) (subset* n o (cdr l))))))
     (else (cons (subset* n o (car l)) (subset* n o (cdr l)))))))
