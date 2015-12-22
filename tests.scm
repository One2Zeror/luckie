(module tests (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  (require "env.scm")
  (require "interp.scm")
  
  (define init-env (lambda () (extend-env 'c 33 (extend-env 'b 22 (extend-env 'a 11 (empty-env)))))) 
  
  (define run
    (lambda (string)
      (test-interp (scan&parse string) init-env)))
  
  (define test-list
    '(
      "a"
      "b"
      "c"
      "- (27, + (33, 22))"
      "- (27, - (33, 22))"
      "- (27, * (1, 22))"
      "- (27, / (33, 11))"
      "- (27, remaider (34, 11))"
      "if 1 {1} else {0}"
      "if 0 {1} else {0}"
      "if zero?(1) {1} else {0}"
      "if zero?(0) {1} else {0}"
      "if equal?(0 1) {1} else {0}"
      "if gt?(0 1) {1} else {0}"
      "if lt?(0 1) {1} else {0}"
      "let a = 6 {a}"
      "let t = 16 {- (t, 11)}"
      "cond [(1,110)(0,200)(3,300)]"
      "cond [(0,110)(1,200)(3,300)]"
      "cond [(0,110)(0,200)(3,300)]"
      "car (cons (1 2))"
      "cdr (cons (1 2))"
      "cdr (cdr (cons (0 cons (1 2))))"
      "null? (emptylist)"
      "null? ('(1))"
      "car ('(1 2 3))"
      "cdr ('(1 2 3))"
      "cdr ('(1 *(2, 3)))"
      "let t = 16 a = 9 {- (t, a)}"
      "let p = func(x) +(x,1) {(p 11)}"
      "let p = func(x) +(x,1) {(p (p 11))}"
	  "(func(x) +(x,2) 2)"
	  "((func (x) func (y) -(x, -(0, y)) 10) 20)"
      "let p = func(x y) -(x,y) {(p (p 8 6) 6)}"
	  "let f = func (x) func (y) *(x, -(0, y)) { ((f 10) 20) }"
	  "let f = func (x) func (y z) *(x, +(+(1, y),z)) { ((f 10) 2 3) }"
	  "let makemult = func (maker) func (x) if zero? (-(x,1)) { 1 } else { *(((maker maker) -(x,1)), x) } 
        { let t = (makemult makemult)  { (t 3) } }"  ;recursive
	  "letrec f(x) = if zero?(x) { 0 } else { +((f -(x,1)), x) }
		{ (f 6) }"
	  "letrec f(x) = if zero?(x) { 0 } else { if zero?(-(x,1)) { 1 } else { +((f -(x,1)), (f -(x,2))) }}
		{ (f 6) }"
		
	  ;;;state
	  " let x = new(22)
		{ let f = func(z)  let zz = new(-(z,@x)) 
		{ @zz } 
		{ -((f 66), (f 55)) } }"
      ))
  
  (define tests
    (lambda () 
      (map run test-list)))
  
  )