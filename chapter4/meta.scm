(define (eval exp env)
    (cond 
        ((self-evaluating? exp) exp)
        ((variable? exp) (look-up-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond-if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else
            (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
    (cond
        ((primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
        (eval-sequence
            (procedure-body procedure)
            (extend-environment
                (procedure-parameters procedure)
                arguments
                (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

; 組み合わせの被演算⼦を引数に取り、各被演算⼦を評価し、対応する値のリストを返します。
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons
            (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
    (if
        (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

; 引数として式の列と環境を取り、出現順に式を評価していきます。返り値は最後の式の値です。
(define (eval-sequence exps env)
    (cond
        ((last-exp? exps) (eval (first-exp exps) env))
        (else
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
    (set-variable-value!
        (assignment-variable exp)
        (eval (assignment-value exp) env)
        env)
    'ok)

(define (eval-defition exp env)
    (define-variable!
        (definition-variable exp)
        (eval (definition-value exp) env)
        env)
    'ok)

; number or string
(define (self-evaluating? exp)
    (cond
        ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define variable? symbol?)

; the quoted-expression format is `(quote <text-of-quotation)`
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quation exp) (cadr exp))
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

; the assignment format is `(set! <var> <value>)`
(define (assignment? exp) (tagged-list? exp 'set))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; the define format is one of the
; - (define <var> <value>)
; - (define (<var> <parameter1> ... <parametern>) <body>)
; - the latest one is the syntax-suger case of the following format.
; - (define <var> (lambda (<parameter1> ... <parametern>) <body>))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        ; (define <var> <value>)
        (caddr exp)
        ; (define (<var> <parameter1> ... <parametern>) <body>)
        ; (cdadr exp) is (<parameter1> ... <parameter>)
        ; (cddr exp) is (<body>)
        (make-lambda (cdadr exp) (cddr exp))))

; (lambda (<parameter1> ... <parametern>) <body>)
(define (lambda? exp) (tagged-list? exp 'lambda))
; (<paramter_1> ... <parameter_n>)
(define (lambda-parameters exp) (cadr exp))
; (<body>)
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

; (if <predicate> <consequent> <alternative>)
(define (if? exp) (tagged-list 'if))
(define (if-predicate exp) (cadr exp))
(define (if-alternative exp)
    (if (null? (cdddr? exp))
        'false
        (cadddr exp)))
(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
    (cond
        ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands exp) (null? exp))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; (cond
;    (<pre_1> <val_1>)
;    (<pre_2> <val_2>)
;    (else <val_else>))

(define (cond? exp) (tagged-list exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        'false ; else 節がない
        (let
            ((first (car clauses))
             (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last: COND -> IF" clauses))
                (make-if
                    (cond-predicate first)
                    (sequence->exp (cond-actions first))
                    (expand-clauses rest))))))

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-lambda p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; 環境に対する演算
; 
; (define (lookup-variable-value var env))
; (define (extend-environment <variables> <values> <base-env>))
; (define (define-variable! <var> <value> <env>))
; (define (set-variables-value! <var> <value> <env>))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
    (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond
                ((null? vars) (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let
                ((frame (first-frame env)))
                (scan
                    (frame-variables frame)
                    (frame-values frame)))))
    (env-loop env))

(define (extend-environment vars vals base-env)
    (if (= length vars) (length vals)
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals)
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals)))))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond
                ((null? vars) (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let
            ((frame (first-frame env)))
            (scan
                (frame-variables frame)
                (frame-values frame)))))
    (env-loop env))

(define (define-variable! var val env)
    (let
        ((frame (first-frame)))
        (define (scan vars vals)
            (cond
                ((null? vars) (add-binding-to-frame! var val frame)) ; 追加
                ((eq? var (car vars)) (set-car! vals val)) ; 上書き
                (else (scan (cdr vars) (cdr vals)))))))


