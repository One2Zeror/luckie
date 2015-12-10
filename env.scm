(module env (lib "eopl.ss" "eopl")
  
  (provide (all-defined-out))
  
  (define empty-env 
    (lambda ()
      '()))
  
  (define extend-env
    (lambda (sym val old_env)
      (cons (cons sym val) old_env)))
  
  (define extend-envs
    (lambda (syms vals old_env)
      (if (not (equal? (length syms) (length vals)))
          (eopl:error "env: sym and value not match")
          (extend-envs-r syms vals old_env))))
  
  (define extend-envs-r  
    (lambda (syms vals old_env)
      (if (null? syms)
          old_env
          (extend-env (car syms) (car vals) (extend-envs-r (cdr syms) (cdr vals) old_env)))))		
  
  (define env-sym
    (lambda (env)
      (caar env)))
  
  (define env-val
    (lambda (env)
      (cdar env)))
  
  (define env-old
    (lambda (env)
      (cdr env)))
  
  ; (define environment?
  ; (lambda (x)
  ; (or (null? x)
  ; (and (pair? x)
  ; (symbol? (car (car x)))
  ; (expval? (cdr (car x)))
  ; (environment? (cdr x))))))
  
  (define apply-env
    (lambda (sym env)
      (if (null? env)
          (eopl:error 'apply-env "No binding for ~s" sym)
          (if (pair? (car env))  
              (apply-sym-env sym env)
              (let [(sym_env (cdddr env))] ;recfun env
                (apply-sym-env sym sym_env))))))
  
  (define apply-sym-env
    (lambda (sym env)
      (let [(esym (env-sym env)) (eval (env-val env))]
        (if (eqv? sym esym)
            eval
            (let [(old_env (env-old env))] (apply-env sym old_env))))))
  
  (define apply-func
    (lambda (sym env)
      (if (null? env)
          (eopl:error 'apply-env "No binding for ~s" sym)
          (let [(esym (env-sym env)) (eval (env-val env))]
            (if (and (eqv? sym esym) (eqv? 'func (car eval)))
                (cdr eval)
                (let [(old_env (env-old env))] (apply-env sym old_env)))))))
  
  )