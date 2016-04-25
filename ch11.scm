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

(define two-in-a-row2
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-help (car lat) (cdr lat))))))

(define two-in-a-row-help
  (lambda (car-lat cdr-lat)
    (cond
     ((null? cdr-lat) #f)
     ((eq? car-lat (car cdr-lat)) #t)
     (else (two-in-a-row2 cdr-lat)))))

