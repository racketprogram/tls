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














