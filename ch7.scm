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
     (else (and (subset? l1 l2) (subset?
                                 l2 l1))))))

(define eqset?
  (lambda (l1 l2)
    (and (subset? l1 l2) (subset? l2 l1))))

(define intersect?1
  (lambda (s1 s2)
    (cond
     ((null? s1) #f)
     ((member? (car s1) s2) #t)
     (else (intersect?1 (cdr s1) s2)))))

(define intersect?
  (lambda (s1 s2)
    (cond
     ((null? s1) #f)
     (else (or (member? (car s1) s2)
               (intersect? (cdr s1) s2))))))

(define intersect
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2)
      (cons (car s1) (intersect (cdr s1) s2)))
     (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
     ((null? s1) s2)
     ((member? (car s1) s2) (union (cdr s1) s2))
     (else (cons (car s1) (union (cdr s1) s2))))))

(define set-diff
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2)
      (set-diff (cdr s1) s2))
     (else (cons (car s1) (set-diff (cdr s1) s2))))))

(define intersectall
  (lambda (set)
    (cond
     ((null? (cdr set)) (car set))
     (else (intersect (car set)
                      (intersectall (cdr set)))))))

(define a-pair?
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l)) #f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
   (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel1
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel1 (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
