(module nameless-interp (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  
  (require "lang.scm")
  (require "translator.scm")
  (require "env.scm")
  (require "nameless-env.scm")
  
  (provide run)
  
  (define run
    (lambda (string)
      (value-of-program
       (translation-of-program
        (scan&parse string)))))
  
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-nameless-env))))))
  
  (define value-of
    (lambda (exp0 nameless_env) 
      (cases expression exp0
        (number-exp (num) (val-num num))
        
        (add-exp (left right) 
                 (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (+ left_num right_num))))
        (sub-exp (left right) 
                 (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (- left_num right_num))))
        (mult-exp (left right) 
                  (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                    (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                      (* left_num right_num))))
        (div-exp (left right) 
                 (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (/ left_num right_num))))
        (remainder-exp (left right) 
                       (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                         (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                           (remainder left_num right_num))))
        
        (zero?-exp (expr) 
                   (let [(val (value-of expr nameless_env))]
                     (let [(val_num (val-num val))]
                       (= 0 val_num))))
        (equal?-exp (left right) 
                    (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                      (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                        (= left_num right_num))))
        (gt?-exp (left right) 
                 (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (> left_num right_num))))
        (lt?-exp (left right) 
                 (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (< left_num right_num))))
        
        (if-exp (test true_exp false_exp)
                (let [(t_val (value-of test nameless_env))]
                  (let [(t (val-boolean t_val))]
                    (if t
                        (value-of true_exp nameless_env)
                        (value-of false_exp nameless_env)
                        ))))
        
        (let-exp (id expr body)
                 (let [(val (value-of expr nameless_env))]
                   (let [(new_env (extend-env id val nameless_env))]
                     (value-of body new_env))))
        
        (cond-exp (conds acts)
                  (val-conds conds acts nameless_env))
        
        (cons-exp (left right) 
                  (let [(left_val (value-of left nameless_env)) (right_val (value-of right nameless_env))]                 
                    (cons left_val right_val)))
        (car-exp (pair)
                 (car (value-of pair nameless_env)))
        (cdr-exp (pair)
                 (cdr (value-of pair nameless_env)))
        (emptylist-exp ()
                       '())
        (null?-exp (expr)
                   (null? (value-of expr nameless_env)))
        (list-exp (exps)
                  (val-list exps nameless_env))
        
        (pack-exp (ids exps)
                  (let [(vals (val-list exps))]
                    (val-pack ids vals)))
        
        (block-exp (exps)
                   (val-list exps nameless_env))
        
        (call-exp (_func args)
                  (if (equal? _func (car nameless_env)) ;recfunc
                      (let [(vals (val-list args nameless_env))]
                        (val-recfunc vals nameless_env))
                      (let [(func (value-of _func nameless_env)) (vals (val-list args nameless_env))]
                        (val-func func vals))))
        
        (letrec-exp (func_name var body letbody)
                    (value-of letbody (cons func_name (cons body (cons var nameless_env)))))
		       
        (nameless-var-exp (n)
                          (apply-nameless-env nameless_env n))
		
        (nameless-let-exp (exps body)
                          (let [(vals (val-list exps nameless_env))]
                            (value-of body (extend-nameless-envs vals nameless_env))))		
		
        (nameless-func-exp (body)
                           (cons body nameless_env))					
        
        (else
         (eopl:error 'value-of 
                     "Illegal expression in translated code: ~s" exp))
        
        )))
  
  ;  (define procedure
  ;    (lambda (var body nameless_env)
  ;      (lambda (val)
  ;        (value-of body (extend-env var val nameless_env)))))
  ;  
  ;  (define-datatype closure closure?
  ;    (closure-val 
  ;     (var symbol?)
  ;     (body expression?)
  ;     (env nameless_env?)
  ;     ))
  
  (define val-num
    (lambda (val)
      (if (number? val)
          val
          (expval-extractor-error 'num val))))
  
  (define val-boolean
    (lambda (val)
      (if (boolean? val)
          val
          (not (zero? val)))))
  
  (define val-conds
    (lambda (conds acts nameless_env)
      (and (not (null? conds))
           (if (val-boolean (value-of (car conds) nameless_env))
               (value-of (car acts) nameless_env)
               (val-conds (cdr conds) (cdr acts) nameless_env))
           )))
  
  (define val-list
    (lambda (exps nameless_env)
      (if (null? exps)
          '()
          (cons (value-of (car exps) nameless_env) (val-list (cdr exps) nameless_env)))))
  
  (define val-pack
    (lambda (ids vals)
      (if (not (equal? (length ids) (length vals)))
          (eopl:error "pack: id and value not match")
          (val-pack-r ids vals))))
  
  (define val-pack-r
    (lambda (ids vals)
      (if (null? ids)
          '()
          '())))
  
  ; (define val-func
  ; (lambda (func args nameless_env)
  ; (value-of (cdr func) (extend-nameless-envs (car func) args nameless_env))))
  
  (define val-func
    (lambda (func vals)
      (let [(body (car func)) (env (cdr func))]
        (value-of body (extend-nameless-envs vals env)))))	
  
  (define val-recfunc
    (lambda (vals nameless_env)
      (let [(func_name (car nameless_env)) (body (cadr nameless_env)) (vars (caddr nameless_env)) (_env (cdddr nameless_env))]
        (value-of body (cons func_name (cons body (cons vars (extend-nameless-envs vars vals _env))))))))	 
  
  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))
  )