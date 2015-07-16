#lang scheme
(require scheme/mpair)

(define (evaluate e env)
  (if (atom? e)
      ;; if expression is an atom
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      ;; if expression is not an atom. check first element for special form.
      ;; note that we do not check here for arity.
      (case (car e)
        ;; if quote, return the second element
        ((quote)  (cadr e))
        ((if)     (if (evaluate (cadr e) env)
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env)))
        ;; this is used for sequencing
        ((begin)  (eprogn (cdr e) env))
        ;; second element is the variable, third element needs to be evaluated.
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        ;; functional application: doesn't have any special operator as
        ;; its first term.
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env))))))

;; update!
;; make-function
;; invoke
;; wrong
;; representing the environment

(define wrong 'wait)

(define (atom? exp)
  (not (pair? exp)))

(define env.init '())

(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          ;; return the final term (definiton of begin)
          ;; by then, it is no longer a pair
          (evaluate (car exps) env))
      '()))

;; Takes a list of expressions and returns the corresponding
;; list of values of those expressiongs
(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))
      '()))

(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-mcdr! (car env) value)
                 value)
          (update! id (cdr env) value))
      (wrong "No such binding" id)))

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (car values)))
             (wrong "Too little values")))
        ((null? variables)
         (if (null? values)
             env
             (wrong "Too many values")))
        ((symbol? variables)
         (cons (cons variables values) env))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))
