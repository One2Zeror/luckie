(module nameless-env (lib "eopl.ss" "eopl")
  
  (provide init-senv extend-senv extend-senvs apply-senv
           init-nameless-env extend-nameless-env extend-nameless-envs apply-nameless-env)
  
  ;; empty-senv : () -> Senv
  (define empty-senv
    (lambda ()
      '()))

  ;;init-senv : () → Senv
  (define init-senv
    (lambda ()
      (extend-senv 'i
                   (extend-senv 'v
                                (extend-senv 'x
                                             (empty-senv))))))  
  
  ;; extend-senv : Var * Senv -> Senv
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))
  
  (define extend-senvs
    (lambda (vars senv)
      (append vars senv)))
  
  ;; apply-senv : Senv * Var -> Lexaddr
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
         (+ 1 (apply-senv (cdr senv) var))))))
  
  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable: ~s" var)))
  
  
  (define empty-nameless-env
    (lambda ()
      '()))

  (define init-nameless-env
    (lambda ()
      (empty-nameless-env)))
  
  (define extend-nameless-env
    (lambda (val nameless-env)
      (cons val nameless-env)))
  
  (define extend-nameless-envs
    (lambda (vals nameless-env)
      (append vals nameless-env)))	  
  
  (define apply-nameless-env
    (lambda (nameless-env n)
      (list-ref nameless-env n)))  
  
 )