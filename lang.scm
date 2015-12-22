(module lang (lib "eopl.ss" "eopl")                
  
  ;; grammar for the func language
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))
  
  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)
      
      (expression (number) number-exp)
      (expression (identifier) var-exp)
      
      (expression
       ("+" "(" expression "," expression ")")
       add-exp)
      (expression
       ("-" "(" expression "," expression ")")
       sub-exp)
      (expression
       ("*" "(" expression "," expression ")")
       mult-exp)
      (expression
       ("/" "(" expression "," expression ")")
       div-exp)
      (expression
       ("remaider" "(" expression "," expression ")")
       remainder-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)
      (expression
       ("equal?" "(" expression expression ")")
       equal?-exp)      
      (expression
       ("gt?" "(" expression expression ")")
       gt?-exp)
      (expression
       ("lt?" "(" expression expression ")")
       lt?-exp)
      
      (expression
       ("if" expression "{" expression "}" "else" "{" expression "}")
       if-exp)
      
      (expression
       ("let" (arbno identifier "=" expression) "{" expression "}")
       let-exp)
      
      (expression
       ("cond" "[" (arbno "(" expression "," expression ")") "]")
       cond-exp)
      
      (expression
       ("cons" "(" expression expression ")")
       cons-exp)
      (expression ("car" "(" expression ")") car-exp)
      (expression ("cdr" "(" expression ")") cdr-exp)
      (expression ("emptylist") emptylist-exp)
      (expression ("null?" "(" expression ")") null?-exp)
      (expression ("'" "(" (arbno expression) ")" ) list-exp)
	  (expression ("list" "(" (arbno expression) ")" ) list-exp)
      
      (expression ("[" (arbno identifier) "]" "=" expression) pack-exp)

      (expression ("{" (arbno expression) "}" ) block-exp)
      
      (expression
       ("func" "(" (arbno identifier) ")" expression)
       func-exp)
      
      (expression
       ("(" expression (arbno expression) ")")
       call-exp)

       (expression
       ("letrec" expression "(" (arbno identifier) ")" "=" expression expression)
       letrec-exp)   
      
      (expression ("%nameless-var" number) nameless-var-exp)
	  
      (expression
        ("%let" (arbno expression) "%" expression)
        nameless-let-exp)
		
      (expression
        ("%func" expression)
        nameless-func-exp)	  

	  ;;;state
      ;;; new for explicit-refs

      ; (expression
        ; ("begin" expression (arbno ";" expression) "end")
        ; begin-exp)

      (expression
        ("new" "(" expression ")")
        newref-exp)

      (expression
        ("@" expression )
        deref-exp)

      (expression
        ("set" "(" expression "," expression ")")
        setref-exp)		
	  
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
