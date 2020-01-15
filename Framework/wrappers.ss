(library (Framework wrappers aux)
  (export
    handle-overflow
    set!
    rewrite-opnds
    code
    jump)
  (import
    (except (chezscheme) set!)
    (Framework match)
    (Framework helpers))

(define int64-in-range?
  (lambda (x)
    (<= (- (expt 2 63)) x (- (expt 2 63) 1))))

(define handle-overflow
  (lambda (x)
    (cond
      [(not (number? x)) x]
      [(int64-in-range? x) x]
      [(not (= x (logand 18446744073709551615 x)))
       (handle-overflow (logand 18446744073709551615 x))]
      [(< x 0) (handle-overflow (+ x (expt 2 64)))]
      [else (handle-overflow (- x (expt 2 64)))])))

(define rewrite-opnds
  (lambda (x)
    (match x
      ;; Begin Haskell hack for disp/index-opnd read/show invariance
      [(disp ,r ,o)
       `(mref ,r ,o)]
      [(index ,r1 ,r2)
       `(mref ,r1 ,r2)]
      [(set! (disp ,r ,o) ,[expr])
       `(mset! ,r ,o ,expr)]
      [(set! (index ,r1 ,r2) ,[expr])
       `(mset! ,r1 ,r2 ,expr)]
      ;; End hack
      [,r (guard (disp-opnd? r))
       `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
      [,r (guard (index-opnd? r))
       `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
      [(set! ,r ,[expr]) (guard (disp-opnd? r))
       `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
      [(set! ,r ,[expr]) (guard (index-opnd? r))
       `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
      [(,[expr*] ...) expr*]
      [,x x])))

(define-syntax set!
  (let ()
    (import (chezscheme))
    (syntax-rules ()
      [(_ x expr)
       (set! x (handle-overflow expr))])))

(define-syntax code
  (lambda (x)
    (define build
      (lambda (body)
        (syntax-case body ()
          [() #'(())]
          [(label expr ...)
           (identifier? #'label)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'(((bounce label))
                (define label
                  (lambda ()
                    (bounce (lambda () expr ...))))
                defn ...))]
          [(expr1 expr ...)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'((expr1 expr ...) defn ...))])))
    (syntax-case x ()
      [(k expr ...)
       (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
         #'((call/cc
              (lambda (bounce)
                defn ...
                expr ...))))])))

(define-syntax jump
  (syntax-rules ()
    [(_ target) (target)]))

)

(library (Framework wrappers)
  (export
    pass->wrapper
    expose-frame-var/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper
    source/wrapper
    verify-scheme/wrapper)
  (import
    (chezscheme)
    (Framework match)
    (Framework GenGrammars l01-verify-scheme)
    (Framework GenGrammars l37-expose-frame-var)
    (Framework GenGrammars l41-flatten-program)
    (Framework helpers)
    (Framework driver)
    (only (Framework wrappers aux) rewrite-opnds))

(define env
  (environment
    '(except (chezscheme) set!)
    '(Framework helpers)
    '(Framework helpers frame-variables)))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

(define-language-wrapper (source/wrapper verify-scheme/wrapper)
  (x)
  (environment env)
  (import (only (Framework wrappers aux) set! handle-overflow))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l01-verify-scheme x) x)))
  ,return-value-register)

(define-language-wrapper expose-frame-var/wrapper
  (x)
  (environment env)
  (import (only (Framework wrappers aux) set! handle-overflow))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds 
	(if (grammar-verification) (verify-grammar:l37-expose-frame-var x) x))))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (environment env)
  (import (only (Framework wrappers aux) set! handle-overflow code jump))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds 
	(if (grammar-verification) (verify-grammar:l41-flatten-program x) x))))
  ,return-value-register)

(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (read in)))

)
