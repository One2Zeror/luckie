(module interp (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  (require "env.scm")
  
  (provide test-interp interp value-of)
  
  (define run
    (lambda (string)
      (test-interp (scan&parse string) init-env)))
  
  (define test-interp 
    (lambda (pgm test_env)
      (cases program pgm
        (a-program (exp0)
                   (value-of exp0 (test_env))))))
  
  (define init-env (lambda () (extend-env 'c 33 (extend-env 'b 22 (extend-env 'a 11 (empty-env)))))) 
  
  (define interp 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp0)
                   (value-of exp0 (init-env))))))
  
  (define value-of
    (lambda (exp0 env) 
      (cases expression exp0
        (number-exp (num) (val-num num))
        (var-exp (var) (apply-env var env))
        
        (add-exp (left right) 
                 (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (+ left_num right_num))))
        (sub-exp (left right) 
                 (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (- left_num right_num))))
        (mult-exp (left right) 
                  (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                    (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                      (* left_num right_num))))
        (div-exp (left right) 
                 (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (/ left_num right_num))))
        (remainder-exp (left right) 
                       (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                         (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                           (remainder left_num right_num))))
        
        (zero?-exp (expr) 
                   (let [(val (value-of expr env))]
                     (let [(val_num (val-num val))]
                       (= 0 val_num))))
        (equal?-exp (left right) 
                    (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                      (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                        (= left_num right_num))))
        (gt?-exp (left right) 
                 (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (> left_num right_num))))
        (lt?-exp (left right) 
                 (let [(left_val (value-of left env)) (right_val (value-of right env))] 
                   (let [(left_num (val-num left_val)) (right_num (val-num right_val))]
                     (< left_num right_num))))
        
        (if-exp (test true_exp false_exp)
                (let [(t_val (value-of test env))]
                  (let [(t (val-boolean t_val))]
                    (if t
                        (value-of true_exp env)
                        (value-of false_exp env)
                        ))))
        ;        (let-exp (id expr body)
        ;                 (let [(val (value-of expr env))]
        ;                   (let [(new_env (extend-env id val env))]
        ;                     (value-of body new_env))))
        (let-exp (ids exps body)
                 (let [(exps_val (val-list exps env))]
                     (value-of body (extend-envs ids exps_val env)))))
					 
        (cond-exp (conds acts)
                  (val-conds conds acts env))
        
        (cons-exp (left right) 
                  (let [(left_val (value-of left env)) (right_val (value-of right env))]                 
                    (cons left_val right_val)))
        (car-exp (pair)
                 (car (value-of pair env)))
        (cdr-exp (pair)
                 (cdr (value-of pair env)))
        (emptylist-exp ()
                       '())
        (null?-exp (expr)
                   (null? (value-of expr env)))
        (list-exp (exps)
                  (val-list exps env))
        
        (pack-exp (ids exps)
                  (let [(vals (val-list exps))]
                    (val-pack ids vals)))
        
        (block-exp (exps)
                   (val-list exps env))
        
        ; (func-exp (args body)
        ; (cons 'func (cons args body)))
        
        ; (call-exp (func_name args)
        ; (let [(func (apply-func func_name env)) (args-val (val-list args env))]
        ; (val-func func args-val env)))
        
        ;(func-exp (var body)
        ;(procedure var body env))
        
        (func-exp (var body)
                  (cons body (cons var env)))
        ;(list var body env))
        
        (call-exp (_func args)
                  (if (equal? _func (car env)) ;recfunc
                      (let [(vals (val-list args env))]
                      (val-recfunc vals env))
                      (let [(func (value-of _func env)) (vals (val-list args env))]
                        (val-func func vals))))
        
        (letrec-exp (func_name var body letbody)
                    (value-of letbody (cons func_name (cons body (cons var env)))))
        
        )))
  
  ;  (define procedure
  ;    (lambda (var body env)
  ;      (lambda (val)
  ;        (value-of body (extend-env var val env)))))
  ;  
  ;  (define-datatype closure closure?
  ;    (closure-val 
  ;     (var symbol?)
  ;     (body expression?)
  ;     (env env?)
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
    (lambda (conds acts env)
      (and (not (null? conds))
           (if (val-boolean (value-of (car conds) env))
               (value-of (car acts) env)
               (val-conds (cdr conds) (cdr acts) env))
           )))
  
  (define val-list
    (lambda (exps env)
      (if (null? exps)
          '()
          (cons (value-of (car exps) env) (val-list (cdr exps) env)))))
  
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
  ; (lambda (func args env)
  ; (value-of (cdr func) (extend-envs (car func) args env))))
  
  (define val-func
    (lambda (func vals)
      (let [(body (car func)) (vars (cadr func)) (env (cddr func))]
        (value-of body (extend-envs vars vals env)))))	
  
  (define val-recfunc
    (lambda (vals env)
      (let [(func_name (car env)) (body (cadr env)) (vars (caddr env)) (_env (cdddr env))]
        (value-of body (cons func_name (cons body (cons vars (extend-envs vars vals _env))))))))	 
  
  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))
  )