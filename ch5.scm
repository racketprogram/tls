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
(define insertL*
  (lambda (n o l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) o) (cons n (cons o (insertL* n o (cdr l)))))
                       (else (cons (car l) (insertL* n o(cdr l))))))
     (else (cons (insertL* n o (car l)) (insertL* n o (cdr l)))))))

(define member**
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (cond
                       ((eq? (car l) a) #t)
                       (else (member** a (cdr l)))))
     (else (or (member** a (car l)) (member** a (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or
                       (eq? (car l) a)
                       (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmostMY
  (lambda (l)
    (cond
     ((null? l) 'noA)
     ((atom? (car l)) (car l))
     (else (leftmostMY (car l))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define epan?
  (lambda (n m)
    (cond
     ((and (number? n) (number? m)) (o= n m))
     ((or (number? n) (number? m)) #f)
     (else (eq? n m)))))

(define eqlist?*
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (epan? (car l1) (car l2)) (eqlist?* (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else (and (eqlist?* (car l1) (car l2)) (eqlist?* (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (epan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? s (car l)) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))

(define rember*simp
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? s (car l)) (cdr l))
     (else (cons (car l) (rember*simp s (cdr l)))))))
