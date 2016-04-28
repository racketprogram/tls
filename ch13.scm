(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define member?
  (lambda (a lat)
    (letrec
        ((mr? (lambda (lat)
                (cond
                 ((null? lat) #f)
                 ((eq? a (car lat)) #t)
                 (else (mr? (cdr lat)))))))
      (mr? lat))))

(define intersect-letrec
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set1)
              (cond
               ((null? set1) '())
               ((member? (car set1) set2)
                (cons (car set1)
                      (I (cdr set1))))
               (else (I (cdr set1)))))))
      (I set1))))

(define intersect-letrec-self
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set1)
              (cond
               ((null? set1) '())
               ((M (car set1) set2)
                (cons (car set1) (I (cdr set1))))
               (else (I (cdr set1))))))
         (M (lambda (a set2)
              (letrec
                  ((N (lambda (set2)
                        (cond
                         ((null? set2) #f)
                         ((eq? a (car set2)) #t)
                         (else (N (cdr set2)))))))
                (N set2)))))
      (I set1))))

(define intersectall
  (lambda (lset)
    (cond
     ((null? (cdr lset)) (car lset))
     (else (intersect (car lset)
                      (intersectall (cdr lset)))))))

(define intersectall-letrec
  (lambda (lset)
    (letrec
        ((I (lambda (lset)
              (cond
               ((null? (cdr lset)) (car lset))
               (else (intersect (car lset)
                                (I (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (I lset))))))

(define intersectall-letcc
  (lambda (lset)
    (call/cc hop
             (letrec
                 ((I (lambda (lset)
                       (cond
                      ((null? (car lset))
                       (hop '()))
                      ((null? (cdr lset)) (car lset))
                      (else (intersect (car lset)
                                       (I (cdr lset))))))))
             (cond
              ((null? lset) '())
              (else (I lset)))))))


(define intersectall-letcc-a
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec
           ((I (lambda (lset)
                 (cond
                  ((null? (car lset))
                   (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (intersect (car lset)
                                   (I (cdr lset))))))))
         (cond
          ((null? lset) '())
          (else (I lset))))))))
