; Written by Jason Laqua and Kaleb Breault

#lang plai
(define-type TJOE
  [num (n number?)]
  [add (lhs TJOE?) (rhs TJOE?)]
  [sub (lhs TJOE?) (rhs TJOE?)]
  [mult (lhs TJOE?) (rhs TJOE?)]
  [div (lhs TJOE?) (rhs TJOE?)]
  [minus (operand TJOE?)]
  [equ (lhs TJOE?) (rhs TJOE?)]
  [lt (lhs TJOE?) (rhs TJOE?)]
  [IF (test TJOE?) (tval TJOE?) (fval TJOE?)]
  [id (name symbol?)]
  [fun (arg-name symbol?) (body TJOE?)]
  [app (fun-expr TJOE?) (arg TJOE?)])

(define-type TJOE-value
  [numV (n number?)]
  [booleanV (b boolean?)]
  [closureV (param symbol?)
            (body TJOE?)
            (ds DefrdSub?)]
  )

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value TJOE-value?) (ds DefrdSub?)]
  )

(define-type Type
  [numType]
  [boolType]
  [funType (domain Type?)
           (codomain Type?)]
  )

;; lookup: symbol DefrdSub --> TJOE
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding found for id ~a" name)]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? name bound-name)
              bound-value
              (lookup name rest-ds))]
 ))

(define (boxed-TJOE-value? b)
  (and (box? b)
       (TJOE-value? (unbox b))
  )
)

(define next-location
  (local ([define last-loc (box -1)])
    (lambda (store)
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))

(define-type TypeEnv
  [mtTypeSub]
  [aTypeSub (name symbol?) 
        (location number?) 
        (typeEnv TypeEnv?)]
)

;; tpyeEnv-lookup: symbol TypeEnv --> Type
(define (typeEnv-lookup name typeEnv)
  (type-case TypeEnv typeEnv
    [mtTypeSub () (error 'typeEnv-lookup "no binding found for id ~a" name)]
    [aTypeSub (bound-name bound-location rest-typeEnv)
          (if (symbol=? name bound-name)
              bound-location
              (typeEnv-lookup name rest-typeEnv))]))

;; tree-add: TJOE-value TJOE-value --> TJOE-value
(define (tree-add a b)
  (numV (+ (numV-n a) (numV-n b)))
 )
 
 ;; tree-sub: TJOE-value TJOE-value --> TJOE-value
 (define (tree-sub a b)
   (numV (- (numV-n a) (numV-n b)))
 )
 
 ;; tree-mult: TJOE-value TJOE-value --> TJOE-value
 (define (tree-mult a b)
   (numV (* (numV-n a) (numV-n b)))
 )

 ;; tree-div: TJOE-value TJOE-value --> TJOE-value
 (define (tree-div a b)
   (numV (quotient (numV-n a) (numV-n b)))
 )
 
 ;; tree-minus: TJOE-value --> TJOE-value
 (define (tree-minus a)
   (numV (- (numV-n a)))
 )
 
 ;; tree-equ: TJOE-value --> TJOE-value
 (define (tree-equ a b)
   (booleanV (eq? (numV-n a) (numV-n b)))
 )
 
 ;; tree-lt: TJOE-value --> TJOE-value
 (define (tree-lt a b)
   (booleanV (< (numV-n a) (numV-n b)))
 )
 
;; interp: TJOE DefrdSub --> TJOE-value
(define (interp expr ds)
  (type-case TJOE expr
    [num (n) (numV n)]
    [add (l r) (tree-add (interp l ds) (interp r ds))]
    [sub (l r) (tree-sub (interp l ds) (interp r ds))]
    [mult (l r) (tree-mult (interp l ds) (interp r ds))]
    [div (l r) (tree-div (interp l ds) (interp r ds))]
    [minus (oper) (tree-minus (interp oper ds))]
    [equ (l r) (tree-equ (interp l ds) (interp r ds))]
    [lt (l r) (tree-lt (interp l ds) (interp r ds))]
    [IF (test tval fval) (cond [(booleanV-b (interp test ds)) (interp tval ds)]
                               [#t (interp fval ds)])]
    [id (v) (lookup v ds)]
    [fun (arg body)(closureV arg body ds)]
    [app (fun-expr arg-expr)
         (let ((fun-closure (interp fun-expr ds)))
           (interp (closureV-body fun-closure)
                          (aSub (closureV-param fun-closure)
                                (interp arg-expr ds)
                                (closureV-ds fun-closure))))]
  ))

;; parse: s-expr -->TJOE
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
                                                      (cond [(eq? 4 (length (second sexp)))
                                                                (cond [(symbol? (first (second sexp)))
                                                                        (cond [(list? (third sexp))
                                                                                (app (fun (first (second sexp))
                                                                                      (parse (third sexp)))
                                                                                 (parse (fourth (second sexp))))]
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
        [(eq? 2 (length sexp)) (app (parse (first sexp)) 
                                   (parse (second sexp)))]
        [else (error 'parse "syntax error")]
     ))

;; type-parse: texp --> type-tree
(define (type-parse texp)
  (cond [(symbol? texp) (cond [(equal? 'number texp) (numType)]
                              [(equal? 'boolean texp) (boolType)]
                              [#t (error 'type-parse "unknown type: ~a" texp)])]
        [(null? texp) (error 'type-parse "texpr is null")]
        [(eq? 3 (length texp)) (cond [(eq? '-> (second texp)) (funType (type-parse (first texp)) (type-parse (third texp)))]
                                     [#t (error 'type-parse "no -> in type expr")]
                               )]
        [#t (error 'type-parse "texp is not of size 3")]
  ))

;; get-type: TJOE --> Type
(define (get-type expr typeEnv)
  (type-case TJOE expr
    [num (n) (numType)]
    [add (l r) (cond [(numType? (get-type l typeEnv)) (cond [(numType? (get-type r typeEnv)) (numType)]
                                                            [#t (error 'get-type "right side of add is of type ~a not number" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of add is of type ~a not number" (get-type l typeEnv))])]
    [sub (l r) (cond [(numType? (get-type l typeEnv)) (cond [(numType? (get-type r typeEnv)) (numType)]
                                                            [#t (error 'get-type "right side of sub is of type ~a not number" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of sub is of type ~a not number" (get-type l typeEnv))])]
    [mult (l r) (cond [(numType? (get-type l typeEnv)) (cond [(numType? (get-type r typeEnv)) (numType)]
                                                            [#t (error 'get-type "right side of mult is of type ~a not number" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of mult is of type ~a not number" (get-type l typeEnv))])]                                                  
    [div (l r) (cond [(numType? (get-type l typeEnv)) (cond [(numType? (get-type r typeEnv)) (numType)]
                                                            [#t (error 'get-type "right side of div is of type ~a not number" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of div is of type ~a not number" (get-type l typeEnv))])]
    [minus (oper) (cond [(numType? (get-type oper typeEnv)) (numType)]
                        [#t (error 'get-type "oper of minus is of type ~a not number" (get-type oper typeEnv))])]
    [equ (l r) (cond [(boolType? (get-type l typeEnv)) (cond [(boolType (get-type r typeEnv)) (boolType)]
                                                            [#t (error 'get-type "right side of eq is of type ~a not bool" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of eq is of type ~a not bool" (get-type l typeEnv))])]
    [lt (l r) (cond [(boolType? (get-type l typeEnv)) (cond [(boolType? (get-type r typeEnv)) (boolType)]
                                                            [#t (error 'get-type "right side of add is of type ~a not bool" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of add is of type ~a not bool" (get-type l typeEnv))])]
    [IF (test tval fval) (cond [(boolType? (get-type test typeEnv)) (cond [(equal? (get-type tval typeEnv) (get-type fval typeEnv)) (get-type tval typeEnv)]
                                                                  [#t (error 'get-type "tval (~a) and fval (~a) types are not the same" (get-type tval typeEnv) (get-type fval typeEnv))])]
                               [#t (error 'get-type "test value of type ~a not bool" (get-type test typeEnv))])]
    [id (v) (typeEnv-lookup v typeEnv)]
    [fun (arg body) (funType (get-type arg typeEnv) (get-type body typeEnv))]
    [app (fun-expr arg-expr) (let ((funtype (typeEnv-lookup fun-expr typeEnv)))
                                  (cond [(funType? funtype) (cond [(equal? (get-type arg-expr typeEnv) (funType-domain funtype)) (funType-codomain funtype)]
                                                                  [#t (error 'get-type "argument of type ~a is not what function ~a expects (~a)" (get-type arg-expr typeEnv) fun-expr (funType-domain funtype))])]
                                        [#t (error 'get-type "type ~a is not of function type" funtype)]))]))

(define (run expr) (interp (parse expr) (mtSub)))

(define (type? expr) (get-type (parse expr) (mtTypeSub)))