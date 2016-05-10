(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons 'cake
                '()))))
