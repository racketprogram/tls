(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x ))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? a (car l)) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (n o l)
      (cond
       ((null? l) '())
       ((test? o (car l))
        (cons n (cons o ((insertL-f test?) n o (cdr l)))))
       (else (cons (car l) ((insertL-f test?) n o (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (n o l)
      (cond
       ((null? l) '())
       ((test? o (car l))
        (cons o (cons n ((insertR-f test?) n o (cdr l)))))
       (else (cons (car l) ((insertR-f test?) n o (cdr l))))))))

(define seqR
  (lambda (n o l)
    (cons o (cons n l))))

(define seqL
  (lambda (n o l)
    (cons n (cons o l))))

(define insertR (insert-g seqR))

(define insertL
  (insert-g
   (lambda (n o l)
     (cons n (cons o l)))))

(define subst
  (lambda (n o l)
    (cond
     ((null? l) '())
     ((eq? (car l) o)
      (cons n (cdr l)))
     (else (cons (car l)
                 (subst n o (cdr l)))))))

(define seqS
  (lambda (n o l)
    (cons n l)))

(define subst (insert-g seqS))

(define rember
  (lambda (a l)
    ((insert-g (lambda (n o l) l)) #f a l)))

(define insert-g
  (lambda (seq)
    (lambda (n o l)
      (cond
       ((null? l) '())
       ((eq? o (car l))
        (seq n o (cdr l)))
       (else (cons (car l) ((insert-g seq) n o (cdr l))))))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x 'o+) o+)
     ((eq? x 'o-) o-)
     (else o^))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
        ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
                 (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

(define a-friend
  (lambda (x y)
    x))
