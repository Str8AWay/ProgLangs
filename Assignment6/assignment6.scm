; Written by Jason Laqua and Kaleb Breault

#lang plai
(define-type JOE+
  [num (n number?)]
  [add (lhs JOE+?) (rhs JOE+?)]
  [sub (lhs JOE+?) (rhs JOE+?)]
  [mult (lhs JOE+?) (rhs JOE+?)]
  [div (lhs JOE+?) (rhs JOE+?)]
  [minus (operand JOE+?)]
  [equ (lhs JOE+?) (rhs JOE+?)]
  [lt (lhs JOE+?) (rhs JOE+?)]
  [with (name symbol?) (named-expr JOE+?) (body JOE+?)]
  [IF (test JOE+?) (tval JOE+?) (fval JOE+?)]
  [id (name symbol?)]
  [fun (arg-name symbol?) (body JOE+?)]
  [rec (bound-id symbol?)
       (named-expr JOE+?)
       (bound-body JOE+?)]
  [app (fun-expr JOE+?) (arg JOE+?)]
  [SET (var symbol?) (value JOE+?)])

(define-type JOE+-value
  [numV (n number?)]
  [booleanV (b boolean?)]
  [closureV (param symbol?)
            (body JOE+?)
            (env Env?)]
)

(define (boxed-JOE+-value? b)
  (and (box? b)
       (JOE+-value? (unbox b))
  )
)

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value JOE+-value?) (env Env?)]
  [aRecSub (name symbol?)
          (value boxed-JOE+-value?)
          (env Env?)]
)

(define (cyclically-bind-and-interp bound-id named-expr env)
  (let* ((value-holder (box (numV 55)))
         (new-env (aRecSub bound-id value-holder env))
         (named-expr-val (interp named-expr new-env)))
     (set-box! value-holder named-expr-val)
     new-env
  ))

;; lookup: symbol Env --> JOE+
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding found for id ~a" name)]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-value rest-env)
          (if (symbol=? name bound-name)
              (unbox boxed-value)
              (lookup name rest-env))]
 ))

;; tree-add: JOE+-value JOE+-value --> JOE+-value
(define (tree-add a b)
  (numV (+ (numV-n a) (numV-n b)))
 )
 
 ;; tree-sub: JOE+-value JOE+-value --> JOE+-value
 (define (tree-sub a b)
   (numV (- (numV-n a) (numV-n b)))
 )
 
 ;; tree-mult: JOE+-value JOE+-value --> JOE+-value
 (define (tree-mult a b)
   (numV (* (numV-n a) (numV-n b)))
 )

 ;; tree-div: JOE+-value JOE+-value --> JOE+-value
 (define (tree-div a b)
   (numV (quotient (numV-n a) (numV-n b)))
 )
 
 ;; tree-minus: JOE+-value --> JOE+-value
 (define (tree-minus a)
   (numV (- (numV-n a)))
 )
 
 ;; tree-equ: JOE+-value --> JOE+-value
 (define (tree-equ a b)
   (booleanV (eq? (numV-n a) (numV-n b)))
 )
 
 ;; tree-lt: JOE+-value --> JOE+-value
 (define (tree-lt a b)
   (booleanV (< (numV-n a) (numV-n b)))
 )
 
;; interp: JOE+ Env --> JOE+-value
(define (interp expr env)
  (type-case JOE+ expr
    [num (n) (numV n)]
    [add (l r) (tree-add (interp l env) (interp r env))]
    [sub (l r) (tree-sub (interp l env) (interp r env))]
    [mult (l r) (tree-mult (interp l env) (interp r env))]
    [div (l r) (tree-div (interp l env) (interp r env))]
    [minus (oper) (tree-minus (interp oper env))]
    [equ (l r) (tree-equ (interp l env) (interp r env))]
    [lt (l r) (tree-lt (interp l env) (interp r env))]
    [IF (test tval fval) (cond [(booleanV-b (interp test env)) (interp tval env)]
                               [#t (interp fval env)])]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  (aSub bound-id
                        (interp named-expr env)
                        env))]
    [rec (bound-id named-expr bound-body)
      (interp bound-body
              (cyclically-bind-and-interp bound-id
                                          named-expr
                                          env))]
    [id (v) (lookup v env)]
    [fun (arg body) (closureV arg body env)]
    [SET (name value) (cyclically-bind-and-interp name value env)]
    [app (fun-expr arg-expr)
         (let ((fun-closure (interp fun-expr env)))
           (interp (closureV-body fun-closure)
                          (aSub (closureV-param fun-closure)
                                (interp arg-expr env)
                                (closureV-env fun-closure))))]
  ))

;; parse: s-expr -->JOE+
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(eq? (first sexp) '*) (cond [(eq? 3 (length sexp)) (mult (parse (second sexp))
                                                                  (parse (third sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for *")])]
        [(eq? (first sexp) '/) (cond [(eq? 3 (length sexp)) (div (parse (second sexp))
                                                                 (parse (third sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for /")])]
        [(eq? (first sexp) '+) (cond [(eq? 3 (length sexp)) (add (parse (second sexp))
                                                                 (parse (third sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for +")])]
        [(eq? (first sexp) '-) (cond [(eq? 2 (length sexp)) (minus (parse (second sexp)))]
                                     [(eq? 3 (length sexp)) (sub (parse (second sexp))
                                                                 (parse (third sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for -")])]
        [(eq? (first sexp) '=) (cond [(eq? 3 (length sexp)) (equ (parse (second sexp))
                                                                 (parse (third sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for =")])]
        [(eq? (first sexp) '<) (cond [(eq? 3 (length sexp)) (lt (parse (second sexp))
                                                                 (parse (third sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for <")])]
        [(eq? (first sexp) 'with) (cond [(eq? 3 (length sexp))
                                            (cond [(list? (second sexp))
                                                      (cond [(eq? 2 (length (second sexp)))
                                                                (cond [(symbol? (first (second sexp)))
                                                                        (cond [(list? (third sexp))
                                                                                (app (fun (first (second sexp))
                                                                                      (parse (third sexp)))
                                                                                 (parse (second (second sexp))))]
;                                                                                (with (first (second sexp))
;                                                                                    (parse (second (second sexp)))
;                                                                                    (parse (third sexp)))]
                                                                              [#t (error 'parse "third item in with expression isn't a list")])]
                                                                      [#t (error 'parse "no symbol for with expression")])]
                                                            [#t (error 'parse "second entry isn't a list")])]
                                                  [#t (error 'parse "second item in with expression isn't a list")])]
                                        [#t (error 'parse "incorrect number of arguments for with")])]
        [(eq? (first sexp) 'if) (cond [(eq? 4 (length sexp)) (IF (parse (second sexp))
                                                                 (parse (third sexp))
                                                                 (parse (fourth sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for if")])]
        [(eq? (first sexp) 'fun) (cond [(eq? 3 (length sexp)) (cond [(and (list? (second sexp)) (eq? 1 (length (second sexp))) (symbol? (car (second sexp))))
                                                                         (fun (car (second sexp))
                                                                              (parse (third sexp)))]
                                                                    [#t (error 'parse "incorrect first operand of fun")])]
                                        [#t (error 'parse "incorrect number of arguments for fun")])]
        [(eq? (first sexp) 'refun) (rec (first (second sexp))
                                    (parse (second (second sexp)))
                                    (parse (third sexp)))]
        [(eq? (first sexp) 'set) (SET (second sexp) (parse (third sexp)))]
        [(eq? 2 (length sexp)) (app (parse (first sexp)) 
                                   (parse (second sexp)))]
        [else (error 'parse "syntax error")]
     ))

(define (run expr) (interp (parse expr) (mtSub)))


