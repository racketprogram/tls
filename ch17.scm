(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m))
              '()))))

(define find
  (lambda (n Ns Rs)
    (letrec ((f (lambda (ns rs)
                  (cond
                   ((null? ns) #f)
                   ((= (car ns) n) (car Rs))
                   (else (f (cdr ns) (cdr rs)))))))
      (f Ns Rs))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m))
                        '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

(define deepM
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result ((lambda (m)
                             (if (zero? m)
                                 'pizza
                                 (cons (deepM (sub1 m)) '())))
                           n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define deepM
  (let  ((Rs '())
         (Ns '()))
    (lambda (n)
      (let ((exists (fins n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (cons (deepM (sub1 n))
                                    '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define counter)

(define consC
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (sub1 m)) '()))))

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(define counter)

(define set-counter)

(define consC
  (let ((N 0))
    (set! counter
          (lambda () N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define deepM
  (let  ((Rs '())
         (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (sub1 n))
                                    '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
               ((null? l) (oh 'no))
               ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (consC (car l)
                          (R (cdr l) oh))))
               (else
                (let ((new-car
                       (call/cc (lambda (oh)
                                  (R (car l) oh)))))
                  (if (atom? new-car)
                      (consC (car l)
                            (R (cdr l) oh))
                      (consC new-car
                            (cdr l)))))))))
      (let ((new-l (call/cc (lambda (oh)
                                (R l oh)))))
        (if (atom? new-l)
            l
            new-l)))))
