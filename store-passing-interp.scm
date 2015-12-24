(module interp (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  (require "env.scm")
  (require "store.scm")
  
  (provide test-interp interp value-of)
  
  ;  (define-datatype answer answer?
  ;    (an-answer
  ;     (val expval?)
  ;     (store store?)))
  
  (define run
    (lambda (string)
      (test-interp (scan&parse string) init-env)))
  
  (define test-interp 
    (lambda (pgm test_env)
      (initialize-store!)   ; for explicit refs.
      (cases program pgm
        (a-program (exp1)
                   (let [(val_store (value-of exp1 test_env (get-store)))]
                     (car val_store)))))) ;(car -> value) (cdr -> store) ; just like answer struct
  
  (define init-env (lambda () (extend-env 'c 33 (extend-env 'b 22 (extend-env 'a 11 (empty-env)))))) 
  
  (define interp 
    (lambda (pgm test_env)
      (initialize-store!)   ; for explicit refs.
      (cases program pgm
        (a-program (exp1)
                   (let [(val_store (value-of exp1 test_env (get-store)))]
                     (car val_store)))))) ;(car -> value) (cdr -> store)
  
  (define value-of
    (lambda (exp0 env store)
      (cases expression exp0
        (number-exp (num)
                    (make-val-store (val-num num) store))
        (var-exp (var) (make-val-store (apply-env var env) store))
        
        (add-exp (left right) 
                 (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                   (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                     (make-val-store (+ left_num right_num) (cdr right_vs)))))
        (sub-exp (left right) 
                 (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                   (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                     (make-val-store (- left_num right_num) (cdr right_vs)))))
        (mult-exp (left right) 
                  (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                    (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                      (make-val-store (* left_num right_num) (cdr right_vs)))))
        (div-exp (left right) 
                 (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                   (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                     (make-val-store (/ left_num right_num) (cdr right_vs)))))
        (remainder-exp (left right) 
                       (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                         (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                           (make-val-store (remainder left_num right_num) (cdr right_vs)))))
        
        (zero?-exp (expr) 
                   (let [(vs (value-of expr env store))]
                     (let [(val_num (val-num (->val vs)))]
                       (make-val-store (= 0 val_num)))))
        (equal?-exp (left right) 
                    (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                      (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                        (make-val-store (= left_num right_num)))))
        (gt?-exp (left right) 
                 (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                   (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                     (make-val-store (> left_num right_num)))))
        (lt?-exp (left right) 
                 (let* [(left_vs (value-of left env store)) (right_vs (value-of right env (->store left_vs)))] 
                   (let [(left_num (val-num (->val left_vs))) (right_num (val-num (->val right_vs)))]
                     (make-val-store (< left_num right_num)))))
        
        (if-exp (test true_exp false_exp)
                (let [(t_vs (value-of test env store))]
                  (let [(t (val-boolean (->val t_vs)))]
                    (if t
                        (value-of true_exp env (->store t_vs))
                        (value-of false_exp env (->store t_vs))
                        ))))
        
        ;        (let-exp (id expr body)
        ;                 (let [(val (value-of expr env))]
        ;                   (let [(new_env (extend-env id val env))]
        ;                     (value-of body new_env))))
        (let-exp (ids exps body)
                 (let [(exps_vs (val-list exps env store))]
                   (value-of body (extend-envs ids (->val exps_vs) env) (->store exps_vs))))
        
        ;        (cond-exp (conds acts)
        ;                  (val-conds conds acts env))
        
        ;        (cons-exp (left right) 
        ;                  (let [(left_val (value-of left env)) (right_val (value-of right env))]                 
        ;                    (cons left_val right_val)))
        ;        (car-exp (pair)
        ;                 (car (value-of pair env)))
        ;        (cdr-exp (pair)
        ;                 (cdr (value-of pair env)))
        ;        (emptylist-exp ()
        ;                       '())
        ;        (null?-exp (expr)
        ;                   (null? (value-of expr env)))
        ;        (list-exp (exps)
        ;                  (val-list exps env))
        ;        
        ;        (pack-exp (ids exps)
        ;                  (let [(vals (val-list exps))]
        ;                    (val-pack ids vals)))
        ;        
        ;        (block-exp (exps)
        ;                   (val-list exps env))
        
        ;        (func-exp (var body)
        ;                  (cons body (cons var env)))
        ;        
        ;        (call-exp (_func args)
        ;                  (if (equal? _func (car env)) ;recfunc
        ;                      (let [(vals (val-list args env))]
        ;                        (val-recfunc vals env))
        ;                      (let [(func (value-of _func env)) (vals (val-list args env))]
        ;                        (val-func func vals))))
        ;        
        ;        (letrec-exp (func_name var body letbody)
        ;                    (value-of letbody (cons func_name (cons body (cons var env)))))
        
        ; (begin-exp (expr exps)
        ; (letrec 
        ; ((value-of-begins
        ; (lambda (e1 es)
        ; (let ((v1 (value-of e1 env)))
        ; (if (null? es)
        ; v1
        ; (value-of-begins (car es) (cdr es)))))))
        ; (value-of-begins expr exps)))
        
        (newref-exp (expr)
                    (let [(vs (value-of expr env store))]
                      (make-val-store (newref (->val vs)) (->store vs)))) ; return position in store
        
        (deref-exp (expr)
                   (let [(vs (value-of expr env store))]
                     (let [(ref (val-num (->val vs)))]
                       (make-val-store(deref ref) (->store vs)))))
        
        (setref-exp (ref expr)
                    (let [(ref (val-num (->val (value-of ref env store))))]
                      (let [(vs (value-of expr env store))]
                        (make-val-store (setref! ref (->val vs)) (->store vs)))))
        
        (else
         (eopl:error 'value-of 
                     "Illegal expression in translated code: ~s" exp0))      
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
    (lambda (exps env store)
      (define val-list-iter
        (lambda (vals exps env store)
           (if (null? exps)
               (cons vals store)
               (let [(vs (value-of (car exps) env store))]
                 (val-list-iter (append vals (list (->val vs))) (cdr exps) env (->store vs))))))
      (val-list-iter '() exps env store)))
  
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
  
  (define make-val-store
    (lambda (val store)
      (cons val store)))
  
  (define ->val
    (lambda (val-store)
      (car val-store)))
  
  (define ->store
    (lambda (val-store)
      (cdr val-store)))
  )