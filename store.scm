(module store (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  
  (provide initialize-store! reference? newref deref setref!
           instrument-newref get-store get-store-as-list)
  
  (define instrument-newref (make-parameter #f))
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.
  
  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  (define the-store 'uninitialized)
  
  ;  ;; empty-store : () -> Sto
  ;  (define empty-store
  ;    (lambda () '()))
  ;  
  ;  ;; initialize-store! : () -> Sto
  ;  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;  (define initialize-store!
  ;    (lambda ()
  ;      (set! the-store (empty-store))))
  ;  
  ;  ;; get-store : () -> Sto
  ;  ;; This is obsolete.  Replaced by get-store-as-list below
  ;  (define get-store
  ;    (lambda () the-store))
  ;  
  ;  ;; reference? : SchemeVal -> Bool
  ;  (define reference?
  ;    (lambda (v)
  ;      (integer? v)))
  ;  
  ;  ;; newref : ExpVal -> Ref
  ;  (define newref
  ;    (lambda (val)
  ;      (let ((next-ref (length the-store)))
  ;        (set! the-store
  ;              (append the-store (list val)))
  ;        (when (instrument-newref)
  ;          (eopl:printf 
  ;           "newref: allocating location ~s with initial contents ~s~%"
  ;           next-ref val))                     
  ;        next-ref)))                     
  ;  
  ;  ;; deref : Ref -> ExpVal
  ;  (define deref 
  ;    (lambda (ref)
  ;      (list-ref the-store ref)))
  ;  
  ;  ;; setref! : Ref * ExpVal -> Unspecified
  ;  (define setref!                       
  ;    (lambda (ref val)
  ;      (set! the-store
  ;            (letrec
  ;                ((setref-inner
  ;                  ;; returns a list like store1, except that position ref1
  ;                  ;; contains val. 
  ;                  (lambda (store1 ref1)
  ;                    (cond
  ;                      ((null? store1)
  ;                       (report-invalid-reference ref the-store))
  ;                      ((zero? ref1)
  ;                       (cons val (cdr store1)))
  ;                      (else
  ;                       (cons
  ;                        (car store1)
  ;                        (setref-inner
  ;                         (cdr store1) (- ref1 1))))))))
  ;              (setref-inner the-store ref)))))
  ;  
  ;  (define report-invalid-reference
  ;    (lambda (ref the-store)
  ;      (eopl:error 'setref
  ;                  "illegal reference ~s in store ~s"
  ;                  ref the-store)))
  ;  
  ;  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;  ;; Exports the current state of the store as a scheme list.
  ;  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;  ;;   where foo, bar, and baz are expvals.
  ;  ;; If the store were represented in a different way, this would be
  ;  ;; replaced by something cleverer.
  ;  ;; Replaces get-store (p. 111)
  ;  (define get-store-as-list
  ;    (lambda ()
  ;      (letrec
  ;          ((inner-loop
  ;            ;; convert store to list as if its car was location n
  ;            (lambda (store n)
  ;              (if (null? store)
  ;                  '()
  ;                  (cons
  ;                   (list n (car store))
  ;                   (inner-loop (cdr store) (+ n 1)))))))
  ;        (inner-loop the-store 0))))
  
  ;;use vector for cost linear time
  (define empty-store
    (lambda()
      (make-vector 0)))
  
  (define init-store
    (lambda()
      (make-vector 10)))
  
  (define initialize-store!
    (lambda()
      (set! the-store (init-store))))
  
  (define get-store
    (lambda()
      the-store))
  
  (define store?
    (lambda (s)
      (cond ((eqv? s 'uninitialized-store) #t)
            ((vector? s) #t)
            (else
             #f))))
  
  (define reference?
    (lambda(x)
      (integer? x)))
  
  (define vector_pos 0)
  
  (define vector-copy!
    (lambda (dest dest-start src src-start length)
      (do ((i 0 (+ i 1)))
        ((= i length))
        (vector-set! dest (+ dest-start i)
                     (vector-ref src (+ src-start i))))))
  
  (define vector-grow
    (lambda(store) 
      (let* ((len (vector-length store))
             (new-store (make-vector (* len 2))))
        (vector-copy! new-store 0 store 0 len)
        new-store)))
  
  (define newref
    (lambda (val)
      (let [(len (vector-length the-store))]
        (unless (= vector_pos len)
          (set! the-store (vector-grow the-store)))
        (vector-set! the-store vector_pos val)
        (unless (instrument-newref)
          (eopl:printf "newref: allocating location ~s with initial contents ~s~%"
                       vector_pos val))
		(let [(pos vector_pos)]
        (set! vector_pos (+ vector_pos 1))
        pos))))
  
  
  (define deref
    (lambda (ref)
      (vector-ref the-store ref)))
  
  (define setref!
    (lambda (ref val)
      (vector-set! the-store ref val)))
  
  (define get-store-as-list
    (lambda()
      (vector->list the-store)))
  
  )