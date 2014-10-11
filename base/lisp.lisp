;
; This is to demonstrate and play with bootstrapping a lisp from our simple
; Python implementation. We could, for instance, implement macros here, or add
; other powerful features.
;


; This is a simple eval function--we need a way to mutate our environment to
; have an eval which functions similarly to the one in pyth.py.
(label eval
  (lambda (expr env)
    (cond
     ((atom expr) (get expr env))
     ((atom (car expr))
      (cond
       ((eq (car expr) (quote quote))  (cadr expr))
       ((eq (car expr) (quote eq))     (eq (eval (cadr expr) env)
                                    (eval (caddr expr) env)))
       ((eq (car expr) (quote atom))   (atom (eval (cadr expr) env)))
       ((eq (car expr) (quote car))    (car (eval (cadr expr) env)))
       ((eq (car expr) (quote cdr))    (cdr (eval (cadr expr) env)))
       ((eq (car expr) (quote cons))   (cons (eval (cadr expr) env)
                                      (eval (caddr expr) env)))
       ((eq (car expr) (quote cond))   (econd (cdr expr) env))
       (else                    (eval (cons (get (car expr) env)
                                            (cdr expr))
                                      env))))
     ((eq (caar expr) (quote label))
      (eval (cons (caddar expr) (cdr expr))
            (assoc env (cadr expr) (caddr expr))))
     ((eq (caar expr) (quote lambda))
      (eval (caddar expr)
            (assoc* env (cadar expr) (eval* (cdr expr) env)))))))

(label eval*
  (lambda (lst env)
    (cond ((car lst) (cons (eval (car lst) env)
                           (eval* (cdr lst) env)))
          (else (quote ())))))

(label econd
  (lambda (clauses env)
    (cond
     ((eval (caar clauses) env) (eval (cadar clauses) env))
     (else (econd (cdr clauses) env)))))

(label apply
  (lambda (fn lst)
    (eval (cons fn lst))))

(label assoc
  (lambda (lst var val)
    (cons (cons var (cons val (quote ()))) lst)))

(label assoc*
  (lambda (lst vars vals)
    (cond ((car vars)
           (assoc* (assoc lst (car vars) (car vals))
                   (cdr vars)
                   (cdr vals)))
          (else lst))))

(label get
  (lambda (var lst)
    (cond
     ((not (car lst)) nil)
     ((eq var (caar lst)) (cadar lst))
     (else (get var (cdr lst))))))

(label caar
  (lambda (lst)
    (car (car lst))))

(label cadar
  (lambda (lst)
    (car (cdr (car lst)))))

(label caddar
  (lambda (lst)
    (car (cdr (cdr (car lst))))))

(label cadr
  (lambda (lst)
    (car (cdr lst))))

(label caddr
  (lambda (lst)
    (car (cdr (cdr lst)))))

(label not
  (lambda (e)
    (cond (e (quote ()))
          (else #t))))
