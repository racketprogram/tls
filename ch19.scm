(define toppings)

(define deepB
  (lambda (m)
    (cond
     ((zero? m)
      (call/cc
       (lambda (jump)
         (set! toppings jump)
         'pizza)))
     (else (cons (deepB (sub1 m))
                 '())))))

(define deep&co
  (lambda (m k)
    (cond
     ((zero? m) (k 'pizza))
     (else
      (deep&co (sub1 m)
               (lambda (x)
                 (k (cons x '()))))))))

(define deep&coB
  (lambda (m k)
    (cond
     ((zero? m)
      (let ()
        (set! toppings k)
        (k 'pizza)))
     (else
      (deep&coB (sub1 m)
                (lambda (x)
                  (k (cons x '()))))))))

(define leave)

(define walk
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (leave (car l)))
     (else
      (let ()
        (walk (car l))
        (walk (cdr l)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define start-it
  (lambda (l)
    (call/cc (lambda (here)
               (set! leave here)
               (walk l)))))

(define fill)

(define waddle
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (let ()
        (call/cc (lambda (rest)
                   (set! fill rest)
                   (leave (car l))))
        (waddle (cdr l))))
     (else (let ()
             (waddle (car l))
             (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (call/cc (lambda (here)
               (set! leave here)
               (waddle l)))))

(define get-next
  (lambda (x)
    (call/cc (lambda (here-again)
               (set! leave here-again)
               (fill 'go)))))

(define get-first
  (lambda (l)
    (call/cc (lambda (here)
               (set! leave here)
               (waddle l)
               (leave '())))))

(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go)))
      (if (atom? n)
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))

(define two-in-a-row**?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 'go)))
               (if (atom? n)
                   (or (eq? n a)
                       (T? n))
                   #f))))
       (get-next (lambda (x)
                   (call/cc (lambda (here-again)
                              (set! leave here-again)
                              (fill 'go)))))
       (fill (lambda (x) x))
       (waddle (lambda (l)
                 (cond
                  ((null? l) '())
                  ((atom? (car l))
                   (let ()
                     (call/cc (lambda (rest)
                                (set! fill rest)
                                (leave (car l))))
                     (waddle (cdr l))))
                  (else (let ()
                          (waddle (car l))
                          (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (call/cc (lambda (here)
                            (set! leave here)
                            (waddle l)))))
        (if (atom? fst)
            (T? fst)
            #f)))))
