(define lots
  (lambda (m)
    (cond
     ((zero? m) '())
     (else (kons 'egg
                 (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (lenkth (kdr l)))))))

(define add-at-end
  (lambda (l)
    (cond
     ((null? (kdr l))
      (konsC (kar l)
             (kons 'egg
                  '())))
     (else (konsC (kar l)
                  (add-at-end (kdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
               ((null? (kdr ls))
                (set-kdr ls
                         (kons 'egg
                               '())))
               (else (A (kdr ls)))))))
      (A l)
      l)))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

(define counter)
(define set-counter)
(define konsC
  (let ((N 0))
    (set! counter
          (lambda () N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))

(define dozen (lots 12))

(define bakers-dozen (add-at-end dozen))

(define bakers-dozen-too (add-at-end-too dozen))

(define eklist?
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else
      (and (eq? (kar ls1) (kar ls2))
           (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

; > (same? dozen bakers-dozen)
; #f
; > (same? dozen bakers-dozen-too)
; #t

(define last-kons
  (lambda (ls)
    (cond
     ((null? (kdr ls)) ls)
     (else (last-kons (kdr ls))))))

(define long (lots 12))

; (set-kdr (last-kons long) long)

(define finite-lenkth
  (lambda (p)
    (call/cc
     (lambda (infinite)
       (letrec
           ((C (lambda (p q)
                 (cond
                  ((same? p q)
                   (infinite #f))
                  ((null? q) 0)
                  ((null? (kdr q)) 1)
                  (else
                   (+ (C (sl p) (qk q))
                      2)))))
            (qk (lambda (x) (kdr (kdr x))))
            (sl (lambda (x) (kdr x))))
         (cond
          ((null? p) 0)
          (else
           (add1 (C p (kdr p))))))))))
