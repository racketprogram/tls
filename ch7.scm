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
(define subset?
  (lambda (l1 l2)
    (cond
     ((null? l1) #t)
     ((member? (car l1) l2) (subset? (cdr l1) l2))
     (else #f))))

(define subset?1
  (lambda (l1 l2)
    (cond
     ((null? l1) #t)
     (else (and (member? (car l1) l2) (subset?1 (cdr l1) l2))))))

(define eqset?noob
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     (else (and (member? (car l1) l2) (eqset?noob (cdr l1) (rember (car l1) l2)))))))

(define eqset?noo
  (lambda (l1 l2)
    (cond
     (else (and (subset?noo l1 l2) (subset?noo l2 l1))))))

(define eqset?
  (lambda (l1 l2)
    (and (subset? l1 l2) (subset? l2 l1))))


