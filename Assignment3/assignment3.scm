#lang plai
(define-type CWAE
  [num (n number?)]
  [add (lhs CWAE?) (rhs CWAE?)]
  [sub (lhs CWAE?) (rhs CWAE?)]
  [mult (lhs CWAE?) (rhs CWAE?)]
  [div (lhs CWAE?) (rhs CWAE?)]
  [minus (operand CWAE?)]
  [if< (expr1 CWAE?) (expr2 CWAE?) (val1 CWAE?) (val2 CWAE?)]
  [with (name symbol?) (named-expr CWAE?) (body CWAE?)]
  [id (name symbol?)]
)

;; parse: s-expr --> CWAE
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
        [(eq? (first sexp) 'with) (cond [(eq? 3 (length sexp))
                                            (cond [(list? (second sexp))
                                                      (cond [(eq? 2 (length (second sexp)))
                                                                (cond [(symbol? (first (second sexp)))
                                                                        (cond [(list? (third sexp))
                                                                                (with (first (second sexp))
                                                                                    (parse (second (second sexp)))
                                                                                    (parse (third sexp)))]
                                                                              [#t (error 'parse "third item in with expression isn't a list")])]
                                                                      [#t (error 'parse "no symbol for with expression")])]
                                                            [#t (error 'parse "second entry isn't a list")])]
                                                  [#t (error 'parse "second item in with expression isn't a list")])]
                                        [#t (error 'parse "incorrect number of arguments for with")])]
        [(eq? (first sexp) 'if<) (cond [(eq? 5 (length sexp)) (if< (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)) (parse (fifth sexp)))]
                                       [#t (error 'parse "incorrect number of arguments for if<")])]
        [else (error (first sexp) "unexpected expression")]
  )
)

;; From Krishnamurthy text, p.19-20
;; subst : CWAE symbol CWAE --> CWAE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument
(define (subst expr sub-id val)
  (type-case CWAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [mult (l r) (mult (subst l sub-id val)
                      (subst r sub-id val))]
    [div (l r) (div (subst l sub-id val)
                    (subst r sub-id val))]
    [minus (oper) (minus (subst oper sub-id val))]
    [if< (expr1 expr2 val1 val2) (if< (subst expr1 sub-id val)
                                      (subst expr2 sub-id val)
                                      (subst val1 sub-id val)
                                      (subst val2 sub-id val))]
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
  (type-case CWAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [mult (l r) (* (calc l) (calc r))]
    [div (l r) (/ (calc l) (calc r))]
    [minus (oper) (- (calc oper))]
    [if< (expr1 expr2 val1 val2) (cond [(< (calc expr1) (calc expr2)) (calc val1)]
                                       [#t (calc val2)])]
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
