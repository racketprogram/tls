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
