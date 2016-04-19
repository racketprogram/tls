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

(define occur-cps
  (lambda (a l cp)
    (cond
     ((null? l) (cp 0 0))
     ((eq? a (car l))
      (occur-cps a
                 (cdr l)
                 (lambda (n o)
                   (cp (add1 n) o))))
     (else (occur-cps a
                      (cdr l)
                      (lambda (n o)
                        (cp n (add1 o))))))))

(define mapcar
  (lambda (cp l)
    (cond
     ((null? l) (cp '()))
     ((atom? (car l))
      (mapcar (lambda (n)
                (cp (cons (car l) n)))
              (cdr l)))
     (else (mapcar (lambda (n)
                     (cp n))
                   (cdr l))))))

(define multiremberLR&co
  (lambda (new oL oR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? oL (car lat))
      (multiremberLR&co new oL oR (cdr lat)
                        (lambda (l n m)
                          (col (cons new (cons (car lat) l))
                               (add1 n)
                               m))))
     ((eq? oR (car lat))
      (multiremberLR&co new oL oR (cdr lat)
                        (lambda (l n m)
                          (col (cons (car lat) (cons new l))
                               n
                               (add1 m)))))
     (else (multiremberLR&co new oL oR (cdr lat)
                             (lambda (l n m)
                               (col (cons (car lat) l)
                                    n
                                    m)))))))

(define even?
  (lambda (n)
    (epan? (o* (o/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 0 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newlat e o)
                          (col (cons (car l) newlat)
                               (add1 e)
                               o))))
       (else (evens-only*&co (cdr l)
                             (lambda (newlat e o)
                               (col newlat
                                    e
                                    (add1 o)))))))
     (else (evens-only*&co (car l)
                           (lambda (al ae ao)
                             (evens-only*&co (cdr l)
                                             (lambda (dl de do)
                                               (col (cons al dl)
                                                    (+ ae de)
                                                    (+ ao do))))))))))

(define occur*&co
  (lambda (a l col)
    (cond
     ((null? l) (col 0))
     ((atom? (car l))
      (cond
       ((eq? a (car l))
        (occur*&co a
                   (cdr l)
                   (lambda (num)
                     (col (add1 num)))))
       (else (occur*&co a
                        (cdr l)
                        (lambda (num)
                          (col num))))))
     (else (occur*&co a
                      (cdr l)
                      (lambda (num1)
                        (occur*&co a
                                   (car l)
                                   (lambda (num2)
                                     (col (o+ num1 num2))))))))))

(define fib
  (lambda (x)
    (cond
     ((eq? x 0) 1)
     ((eq? x 1) 1)
     (else (+ (fib (- x 2))
              (fib (- x 1)))))))

(define fib&co
  (lambda (x col)
    (cond
     ((eq? x 0) (col 1))
     ((eq? x 1) (col 1))
     (else (fib&co (- x 1)
                   (lambda (num1)
                     (fib&co (- x 2)
                             (lambda (num2)
                               (col (+ num1 num2))))))))))
