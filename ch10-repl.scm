;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;REPL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
