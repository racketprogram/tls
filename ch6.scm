(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote o+))
      (and
       (numbered? (car aexp))
       (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote o*))
      (and
       (numbered? (car aexp))
       (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote o^))
      (and
       (numbered? (car aexp))
       (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) 'o+)
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o-)
      (o- (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o*)
      (o* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o/)
      (o/ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))
