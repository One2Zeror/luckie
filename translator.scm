(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  (require "nameless-env.scm")
  
  (provide translation-of-program)  
  
  
  ;; translation-of-program : Program -> Nameless-program
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (a-program
                    (translation-of exp1 (init-senv)))))))
  
  ;;translation-of : Exp × Senv → Nameless-exp
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        (number-exp (num) (number-exp num))
        (var-exp (var)
                 (nameless-var-exp
                  (apply-senv senv var)))
        (add-exp (left right)
                 (add-exp
                  (translation-of left senv)
                  (translation-of right senv)))
        (sub-exp (left right)
                 (sub-exp
                  (translation-of left senv)
                  (translation-of right senv)))
        (mult-exp (left right)
                  (mult-exp
                   (translation-of left senv)
                   (translation-of right senv)))
        (div-exp (left right)
                 (div-exp
                  (translation-of left senv)
                  (translation-of right senv)))
        (remainder-exp (left right)
                       (remainder-exp
                        (translation-of left senv)
                        (translation-of right senv)))
        (zero?-exp (exp1)
                   (zero?-exp
                    (translation-of exp1 senv)))
        
        (emptylist-exp ()
                       (emptylist-exp))
        
        (car-exp (body)
                 (car-exp (translation-of body senv)))
        (cdr-exp (body)
                 (cdr-exp (translation-of body senv)))
        (null?-exp (exp)
                   (null?-exp (translation-of exp senv)))
        
        (if-exp (exp1 exp2 exp3)
                (if-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)
                 (translation-of exp3 senv)))
        
        (list-exp (exps)
                  (list-exp
                   (trans-list exps senv)))		
        
        (let-exp (vars exps body)
                 (nameless-let-exp
                  (trans-list exps senv)
                  (translation-of body
                                  (extend-senvs vars senv))))
        (func-exp (vars body)
                  (nameless-func-exp
                   (translation-of body
                                   (extend-senvs vars senv))))
        (call-exp (func args)
                  (call-exp
                   (translation-of func senv)
                   (trans-list args senv)))
        (else
         (report-invalid-source-expression exp)))))
  
  (define trans-element
    (lambda (senv)
      (lambda (elem)
        (translation-of elem senv))))
  
  (define trans-list
    (lambda (exps senv)
      (map (trans-element senv) exps)))
  
  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of
                  "invalid expression in source code: ~s" exp)))
  
  
  )