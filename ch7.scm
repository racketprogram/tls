(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makesetMY
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     ((member? (car l1) l2) (makesetMY (cdr l1) l2))
     (else (makesetMY (cdr l1) (cons (car l1) l2))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset1
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
                  (makeset1
                   (multirember (car lat) (cdr lat))))))))

