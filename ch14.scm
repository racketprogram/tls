(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define leftmost-a
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     ((atom? (leftmost (car l)))
      (leftmost (car l)))
     (else (leftmost (cdr l))))))

(define leftmost-let
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else (let ((a (leftmost-let (car l))))
             (cond
              ((atom? a) a)
              (else (leftmost (cdr l)))))))))

(define rember1*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? a (car l)) (cdr l))
       (else (cons (car l)
                   (rember1* a (cdr l))))))
     (else
      (cond
       ((eqlist?
         (rember1* a (car l))
         (car l))
        (cons (car l)
              (rember1* a (cdr l))))
       (else (cons (rember1* a (car l))
                   (cdr l))))))))


(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define rember1*-letrec
  (lambda (a l)
    (letrec
        ((RR (lambda (l)
               (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                  ((eq? a (car l)) (cdr l))
                  (else (cons (car l)
                              (RR (cdr l))))))
                (else
                 (cond
                  ((eqlist? (RR (car l))
                            (car l))
                   (cons (car l)
                         (RR (cdr l))))
                  (else (cons (RR (car l))
                              (cdr l)))))))))
      (RR l))))

(define rember1*-letrec-let
  (lambda (a l)
    (letrec
        ((RR (lambda (l)
               (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                  ((eq? (car l) a)
                   (cdr l))
                  (else (cons (car l)
                              (RR (cdr l))))))
                (else
                 (let ((v (RR (car l))))
                   (cond
                    ((eqlist? (car l) v)
                     (cons v (RR (cdr l))))
                    (else (cons v (cdr l))))))))))
      (RR l))))

(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth* (cdr l)))
     (else
      (cond
       ((> (depth* (cdr l))
           (add1 (depth* (car l))))
        (depth* (cdr l)))
       (else
        (add1 (depth* (car l)))))))))

(define depth*-let
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l))
      (depth*-let (cdr l)))
     (else
      (let ((d (depth*-let (cdr l)))
            (a (add1 (depth*-let (car l)))))
        (if (> d a) d a))))))

(define depth*-let2
  (lambda (l)
    (cond
     ((null? l) 1)
     (else
      (let ((d (depth*-let2 (cdr l))))
        (cond
         ((atom? (car l)) d)
         (else
          (let ((a (add1 (depth*-let2 (car l)))))
            (cond
             ((> d a) d)
             (else a))))))))))

(define leftmost-let-call/cc
  (lambda (l)
    (call/cc
     (lambda (cc)
       (letrec ((lm (lambda (l)
                      (cond
                       ((null? l) '())
                       ((atom? (car l)) (cc (car l)))
                       (else (let ()
                               (lm (car l))
                               (lm (cdr l))))))))
         (lm l))))))

(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l)
                (rm a (cdr l) oh))))
     (else
      (if (atom?
           (call/cc
            (lambda (oh)
              (rm a (car l) oh))))
          (cons (car l)
                (rm a (cdr l) oh))
          (cons (rm a (car l) 0)
                (cdr l)))))))

(define rm-let
  (lambda (a l cc)
    (cond
     ((null? l) (cc 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l)
                (rm-let a (cdr l) cc))))
     (else
      (let ((new-l (call/cc (lambda (cc) (rm-let a (car l) cc)))))
        (if (atom? new-l)
            (cons (car l)
                  (rm-let a (cdr l) cc))
            (cons new-l (cdr l))))))))


(define rember1*-call/cc
  (lambda (a l)
    (let ((new-l (call/cc (lambda (say) (rm-let a l say)))))
      (if (atom? new-l)
          l
          new-l))))

(define rm-try
  (lambda (a l cc)
    (cond
     ((null? l) (cc 'no))
     ((atom? (car l))
      (if (eq? (car l) a)
          (cdr l)
          (cons (car l)
                (rm-try a (cdr l) cc))))
     (else
      (call/cc (lambda (cc1)
                 (call/cc (lambda (cc2)
                            (cc1 (cons (rm-try a (car l) cc2)
                                       (cdr l)))))
                 (cons (car l)
                       (rm-try a (cdr l) cc))))))))

(define rember1*-try
  (lambda (a l)
    (call/cc (lambda (cc1)
               (call/cc (lambda (cc2)
                          (cc1 (rm-try a l cc2))))
               l))))
