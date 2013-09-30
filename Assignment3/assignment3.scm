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

;; parse: s-expr --> WAE
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
        [ else (error 'parse "unexpected expression")]
  )
)

;; From Krishnamurthy text, p.19-20
;; subst : WAE symbol WAE --> WAE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument
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
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]
  )
)

(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [mult (l r) (* (calc l) (calc r))]
    [div (l r) (/ (calc l) (calc r))]
    [with (s nexpr body) (calc (subst body s (num (calc nexpr))))]
    [id (s) (error 'calc "unexpected free variable encountered")]
  )
)

;;(calc (parse '{with {x 3} {+ 2 x}}))
;;(calc (parse '{with {x 3} {with {y 11} {+ y x}}}))

;;Why "(if (symbol=? bound-id sub-id)"?:
;;(calc (parse '{with {x 3} {with {x 11} {+ x x}}}))

;;(calc (parse '{with {x 3} {with {y {+ x 1}} {+ y x}}}))
;;Hint: consider (subst (parse '{with {y {+ x 1}} {+ y x}}) 'x (num 3))

;;(calc (parse '{with {x 5} {with {x x} {+ x 1}}}))
;;(subst (parse '{with {x x} {+ x 1}}) 'x (num 5))
