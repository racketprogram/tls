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
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o^)
      (o^ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))
(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) 'o+)
      (o+ (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'o-)
      (o- (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'o*)
      (o* (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp)))))))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value3
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o-)
      (o- (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o/ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o/)
      (o/ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'o^)
      (o^ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define oo+
  (lambda (n m)
    (cond
     ((null? n) m)
     (else (edd1 (oo+ (zub1 n) m))))))

