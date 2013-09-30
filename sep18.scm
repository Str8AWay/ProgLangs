#lang plai
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [mult (lhs WAE?) (rhs WAE?)]
  [div (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
  )

(define (calc a-wae)
  (type-case WAE a-wae
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [mult (l r) (* (calc l) (calc r))]
    [div (l r) (/ (calc l) (calc r))]
    [with (name nexpr body) (calc (subst-all (calc nexpr) name body))]
    [id (name) 666]
   ))

(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(eq? (first sexp) '*) (mult (parse (second sexp))
                                     (parse (third sexp)))]
        [(eq? (first sexp) '/) (div (parse (second sexp))
                                     (parse (third sexp)))]
        [(eq? (first sexp) '+) (add (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) '-) (sub (parse (second sexp))
                                    (parse (third sexp)))]
        [(eq? (first sexp) 'with) (with (first (second sexp))
                                        (parse (second (second sexp)))
                                        (parse (third sexp)))]
        [else (error 'parse "unexpected expression")]
  )
)

(define (subst-all new old lat)
  (cond [(null? lat) '()]
        [(equal? (car lat) old) (cons new (subst-all new old (cdr lat)))]
        [#t (cons (car lat) (subst-all new old (cdr lat)))]
  )
)

(define (subst expr sub-id val)
    (type-case WAE expr
        [num (n) expr]
        [add (l r) (add (subst l sub-id val)
                        (subst r sub-id val))]
        [sub (l r) (sub (subst l sub-id val)
                        (subst r sub-id val))]
        [mult (l r) (mult (subst l sub-id val)
                          (subst r sub-id val))]
        [div (l r) (div (subst l sub-id val)
                        (subst r sub-id val))]
        [with (bound-id named-expr bound-body)
              (if (symbol=? bound-id sub-id)
                  expr
                  (with bound-id
                        named-expr
                        (subst bound-body sub-id val)))]
        [id (v) (if (symbol=? v sub-id) val expr)]
    )
)
