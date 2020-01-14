(library (Compiler generate-x86-64)
	 (export generate-x86-64)
	 (import
	  (chezscheme)
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers))

	 #|
	 Asssignment 1
	 Scheme Subset
	 Program -> (begin Statement+)
	 Statement -> (set! Var1 int64)
	 |  (set! Var1 Var2)
	 |  (set! Var1 (Binop Var1 int32))
	 |  (set! Var1 (Binop Var1 Var2))
	 Var -> rax | rcx | rdx | rbx | rbp | rsi | rdi
	 |  r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
	 Binop -> + | - | *
	 |#

	 (define-who (generate-x86-64 exp)
	   ;; binop
	   (define (binop op)
	     (match op
		    [+ 'addq]
		    [- 'subq]
		    [* 'imulq]
		    [,x (errorf who "invalid binop ~s" x)]))
	   
	   (define (gene code) 
	     (match code
		    ;; 生成代码 (set! dst (binop dst src))
		    ;; 比如 (set! rax (+ rax 10))  会生成 addq $10, %rax
		    [(set! ,dst (,op ,dst ,src))
		     (emit (binop op) src dst)]

			;;  生成代码 (set! Var1 Var2) 或者  (set! Var1 int64)
			;; 比如 (set! rax 5) 会生成 movq $5, %rax
		    [(set! ,dst ,src)
		     (emit 'movq src dst)]
		    
		    
		    [,x (errorf who "invalid syntax ~s" x)]))

	   (match exp
		  [(begin ,exp ...)(emit-program (for-each gene exp))]
		  [,x (errorf who "invalid program ~s" x)]
		  )
	   )
	 )