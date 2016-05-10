(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake
                '()))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons 'cake
                '()))))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients
          (cons food ingredients))
    (cons food
          (cons 'cake
                '()))))

(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else (cons (deep (sub1 m)) '())))))

(define Ns '())

(define Rs '())

(define deepR
  (lambda (n)
    (set! Rs (cons (deep n) Rs))
    (set! Ns (cons n Ns))
    (deep n)))

(define deepR-let
  (lambda (n)
    (let ((d (deep n)))
      (set! Rs (cons d Rs))
      (set! Ns (cons n Ns))
      d)))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((f (lambda (ns rs)
              (cond
               ((= (car ns) n) (car rs))
               (else (f (cdr ns) (cdr rs)))))))
      (f Ns Rs))))

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons result Rs))
          result))))

(define deep
  (lambda (n)
    (cond
     ((zero? n) 'pizza)
     (else (cons (deepM (sub1 n)) '())))))

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons result Rs))
            result)))))

(define find
  (lambda (n Ns Rs)
    (letrec ((f (lambda (ns rs)
                  (cond
                   ((null? ns) #f)
                   ((= (car ns) n) (car Rs))
                   (else (f (cdr ns) (cdr rs)))))))
      (f Ns Rs))))

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define length
  (lambda (l)
    0))

(set! length
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (h (cdr l)))))))
    h))

(define L
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (L (lambda (arg) (h arg))))
    h))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) 0)))
      (set! h
            (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

(define length (Y! L))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x)
             ((f f) x)))))))

(define length (Y L))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) 0)))
      (set! h
            (L (lambda (arg) (h arg))))
      h)))
