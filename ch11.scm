(define two-in-a-row
  (lambda (lat)
    (cond
     ((null? lat) #f)
     ((two-test (car lat) (cdr lat)) #t)
     (else (two-in-a-row (cdr lat))))))

(define two-test
  (lambda (car-lat cdr-lat)
    (cond
     ((null? cdr-lat) #f)
     ((eq? car-lat (car cdr-lat)) #t)
     (else (two-test car-lat (cdr cdr-lat))))))

(define two-in-a-row^
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-help^ (car lat) (cdr lat))))))

(define two-in-a-row-help^
  (lambda (car-lat cdr-lat)
    (cond
     ((null? cdr-lat) #f)
     ((eq? car-lat (car cdr-lat)) #t)
     (else (two-in-a-row^ cdr-lat)))))

(define sum-of-prefixes^
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (sum-of-prefixes-help^ (car lat) (cdr lat))))))

(define sum-of-prefixes-help^
  (lambda (car-lat cdr-lat)
    (cond
     ((null? cdr-lat) (cons car-lat (quote ())))
     (else (cons car-lat (sum-of-prefixes-help^ (+ car-lat (car cdr-lat))
                                                (cdr cdr-lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((null? lat) 'error)
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) (quote ()))
     (else
      (cons (pick (car tup)
                  (cons (car tup) rev-pre))
            (scramble-b (cdr tup)
                        (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))

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
