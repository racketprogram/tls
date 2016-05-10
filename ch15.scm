(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(set! x 'ya)

(define gobbler
  (let ((x 'ya))
    (lambda (food)
      (set! x food)
      (cons x (cons food '())))))

(define gobbler2
  (lambda (food)
    (let ((x 'ya))
      (set! x food)
      (cons x (cons food '())))))
