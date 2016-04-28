(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             ((null? lat) (quote ()))
             ((eq? a (car lat))
              (mr (cdr lat)))
             (else (cons (car lat)
                         (mr (cdr lat))))))))
     lat)))

(define multirember-letrec
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                 ((null? lat) '())
                 ((eq? a (car lat))
                  (mr (cdr lat)))
                 (else (cons (car lat)
                             (mr (cdr lat))))))))
       mr)
     lat)))

(define multirember-rec
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                ((null? lat) '())
                ((eq? a (car lat))
                 (mr (cdr lat)))
                (else (cons (car lat)
                            (mr (cdr lat))))))))
      (mr lat))))

(define mm
  (lambda (x)
    x))

(define test-mm
  (lambda (a x)
    (letrec
        ((mm
          (lambda (x)
            x)))
      (mm x))))

(define test-mm-Y
  (lambda (a x)
    ((Y (lambda (mm)
          (lambda (x)
            x)))
     x)))

(define multirember-f
  (lambda (test?)
    (letrec
        ((ml-f (lambda (a lat)
                 (cond
                  ((null? lat) '())
                  ((test? a (car lat))
                   (ml-f a (cdr lat)))
                  (else (cons (car lat)
                              (ml-f a (cdr lat))))))))
      ml-f)))

(define member?
  (lambda (a lat)
    (letrec
        ((mr? (lambda (lat)
                (cond
                 ((null? lat) #f)
                 ((eq? a (car lat)) #t)
                 (else (mr? (cdr lat)))))))
      (mr? lat))))

(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set1)
                  (cond
                   ((null? set1) set2)
                   ((MR? (car set1) set2)
                    (U (cdr set1)))
                   (else (cons (car set1) (U (cdr set1)))))))
             (MR? (lambda (a lat)
                    (letrec ((M (lambda (lat)
                                  (cond ((null? lat) #f)
                                        ((eq? a (car lat)) #t)
                                        (else (M (cdr lat)))))))
                      (M lat)))))
      (U set1))))

(define two-in-a-row-protect
  (letrec
      ((W (lambda (a lat)
            (cond
             ((null? lat) #f)
             ((eq? a (car lat)) #t)
             (else (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(define sum-of-prefixes-protect
  (lambda (tup)
    (letrec ((S (lambda (sum tup)
                  (cond
                   ((null? tup) '())
                   (else (cons (+ sum (car tup))
                               (S (+ sum (car tup))
                                  (cdr tup))))))))
      (S 0 tup))))


