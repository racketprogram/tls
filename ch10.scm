(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-entry-help name
                        (first entry)
                        (second entry)
                        entry-f)))

(define lookup-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? name (car names))
           (car values))
          (else (lookup-entry-help name
                                   (cdr names)
                                   (cdr values)
                                   entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
                            (car table)
                            (lambda (name)
                              (lookup-in-table name
                                               (cdr table)
                                               (table-f))))))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))


(define atom-to-action
  (lambda (e)
    (cond
     ((number? e)             *const)
     ((eq? e #t)              *const)
     ((eq? e #f)              *const)
     ((eq? e (quote cons))    *const)
     ((eq? e (quote car))     *const)
     ((eq? e (quote cdr))     *const)
     ((eq? e (quote null?))   *const)
     ((eq? e (quote eq?))     *const)
     ((eq? e (quote atom?))   *const)
     ((eq? e (quote zero?))   *const)
     ((eq? e (quote add1))    *const)
     ((eq? e (quote sub1))    *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote)) *quote)
       ((eq? (car e) (quote lambda))     *lambda)
       ((eq? (car e) (quote cond))  *cond)
       (else *application)))
     (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (cons name (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitibe)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (anwser-of (car lines))
               table))
     ((meaning (question-of (car lines))
               table)
      (meaning (anwser-of (car lines))
               table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))

(define question-of first)

(define anwser-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-line-of e) table)))

(define cond-line-of cdr)

(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else (cons (meaning (car args) table)
                 (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply1
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply1
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive
       (second fun) vals))
     ((non-primitive? fun)
      (apply-closure
       (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons))
      (cons (first vals) (second vals)))
     ((eq? name (quote car))
      (car (first vals)))
     ((eq? name (quote cdr))
      (cdr (first vals)))
     ((eq? name (quote null?))
      (null? (first vals)))
     ((eq? name (quote eq?))
      (eq? (first vals) (second vals)))
     ((eq? name (quote atom?))
      (:atom? (first vals)))
     ((eq? name (quote zero?))
      (zero? (first vals)))
     ((eq? name (quote add1))
      (add1 (first vals)))
     ((eq? name (quote sub1))
      (sub1 (first vals)))
     ((eq? name (quote number?))
      (number? (first vals)))
     ((eq? name (quote +))
      (+ (first vals) (second vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive))
      #t)
     ((eq? (car x) (quote non-primitive))
      #t)
     (else #f))))


(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (repl evaluator)
  (display "repl>")
  (let ((expr (read)))
    (cond ((eq? expr 'exit)
           (display "Exiting read-eval-print loop")
           (newline))
          (else
           (write (evaluator expr))
           (newline)
           (repl evaluator)))))

(repl value)