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
  [fun (arg-name symbol?) (body TJOE?) (type Type?)]
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

(define-type TypeEnv
  [mtTypeSub]
  [aTypeSub (name symbol?)
        (type Type?)
        (typeEnv TypeEnv?)]
)

;; typeEnv-lookup: symbol TypeEnv --> Type
(define (typeEnv-lookup name typeEnv)
  (type-case TypeEnv typeEnv
    [mtTypeSub () (error 'lookup "no binding found for id ~a" name)]
    [aTypeSub (bound-name bound-value rest-typeEnv)
          (if (symbol=? name bound-name)
              bound-value
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
    [fun (arg body type)(closureV arg body ds)]
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
                                                                                (app (fun (first (second sexp))
                                                                                      (parse (third sexp)) (type-parse (third (second sexp))))
                                                                                 (parse (fourth (second sexp))))]
                                                                      [#t (error 'parse "no symbol for with expression")])]
                                                            [#t (error 'parse "second entry isn't a list of length 4")])]
                                                  [#t (error 'parse "second item in with expression isn't a list")])]
                                        [#t (error 'parse "incorrect number of arguments for with")])]
        [(eq? (first sexp) 'if) (cond [(eq? 4 (length sexp)) (IF (parse (second sexp))
                                                                 (parse (third sexp))
                                                                 (parse (fourth sexp)))]
                                     [#t (error 'parse "incorrect number of arguments for if")])]
        [(eq? (first sexp) 'fun) (cond [(eq? 3 (length sexp)) (cond [(and (list? (second sexp)) (eq? 3 (length (second sexp))) (symbol? (car (second sexp))))
                                                                         (fun (car (second sexp))
                                                                              (parse (third sexp))
                                                                              (type-parse (third (second sexp))))]
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
    [equ (l r) (cond [(numType? (get-type l typeEnv)) (cond [(numType? (get-type r typeEnv)) (boolType)]
                                                            [#t (error 'get-type "right side of eq is of type ~a not bool" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of eq is of type ~a not bool" (get-type l typeEnv))])]
    [lt (l r) (cond [(numType? (get-type l typeEnv)) (cond [(numType? (get-type r typeEnv)) (boolType)]
                                                            [#t (error 'get-type "right side of add is of type ~a not bool" (get-type r typeEnv))])]
                     [#t (error 'get-type "left side of add is of type ~a not bool" (get-type l typeEnv))])]
    [IF (test tval fval) (cond [(boolType? (get-type test typeEnv)) (cond [(equal? (get-type tval typeEnv) (get-type fval typeEnv)) (get-type tval typeEnv)]
                                                                  [#t (error 'get-type "tval (~a) and fval (~a) types are not the same" (get-type tval typeEnv) (get-type fval typeEnv))])]
                               [#t (error 'get-type "test value of type ~a not bool" (get-type test typeEnv))])]
    [id (v) (typeEnv-lookup v typeEnv)]
    [fun (arg body type) (funType type (get-type body (aTypeSub arg
                                                                  type
                                                                  typeEnv)))] 
    [app (fun-expr arg-expr) (let ((funtype (get-type fun-expr typeEnv)))
                                  (cond [(funType? funtype) (cond [(equal? (get-type arg-expr typeEnv) (funType-domain funtype)) (funType-codomain funtype)]
                                                                  [#t (error 'get-type "argument of type ~a is not what function ~a expects (~a)" (get-type arg-expr typeEnv) fun-expr (funType-domain funtype))])]
                                        [#t (error 'get-type "type ~a is not of function type" funtype)]))]))

(define (run expr) (interp (parse expr) (mtSub)))

(define (type? expr) (get-type (parse expr) (mtTypeSub)))


;;; Test Problems ;;;
(type? '{with {x : number 2}
  {with {y : number 12}
    {+ {* x x} {* y y}}
}})



(type? '{- 1 {- 2 {- 3 {- 4 {- 5 6}}}}})



(type? '{with {x : number 3} ;; this should evaluate to a boolean value
  {= x 2}}
  )
  
  
(type? '{with {x : number 3}
  {= x 3}}
  )
(type? '{with {not : (boolean -> boolean)
         {fun {v : boolean}
           {if v {= 0 1} {= 0 0}}
      }  }
   {with {limit : number 44}
      {with {flag : boolean {< 70 limit}}
        {not flag}
   }  }
})
  
(type? '{if {= {* 5 21} {* 7 15}}
    9999
    5555
}
)

;; A silly example:
;(type? '{with {number {fun {number : number}
;        {* number number}}}
;   {with {num : number 8}
;     {number num} }}
;)

(type? '{with {x : number 9}    ; you can change these vals, but the larger one should
  {with {y : number 3}  ; always wind up at the left of the final number
               ; and the smaller one at the right
     {with  {min : number  {if {< x y}
                     x 
                     y}}
        {with  {max : number  {if {< x y}
                        y 
                        x}}
           {+ {* max 1000} min} ; output will be best if smaller is
 }}}}                           ; no more than two digits
)

(type? '{with {double : (number -> number) {fun {n : number} {* 2 n}}}
  {double 12}}
)

(type? '{with {abs : (number -> number) {fun {x : number} {if {< x 0} {- x} x}}} ;; absolute value
   {abs -101}
}
)

;; Use currying to define "two-parameter"
;; function in terms of one-parameter
;; (first-class) functions
;;
;;  {{mod-base b} n}
;;
;; returns n modulo b.
;; [This should work in the language TJOE]
;;
;; written by mike slattery - nov 2013
;;;;;;;;;;;;;;;;;;;;;;;;;

(type? '{with {mod-base : (number -> (number -> number)) {fun {b : number}
                  {fun {n : number}
                    {- n {* b {/ n b}}}
                  }
                }
      }
  {{mod-base 7} 11} ;; Compute 11 mod 7
}
)

;; Or put the parameters in the more
;; traditional order: 

(type? '{with {mod : (number -> (number -> number)) {fun {n : number}
                  {fun {b : number}
                    {- n {* b {/ n b}}}
                  }
                }
      }
  {{mod 100} 7} ;; Compute 100 mod 7
}
)

;----------------------------

;Invalid programs the parser shouldn't like:

;(type? '{+ 3 4 5})

;(type? '{with {x:number 3}
;   x})
   
;(type? '{with {x 3}
;   x})

;(type? '{6 - 7})

;(type? '{fun {n : number 12}
;   {+ n 3}})
   
;(type? '{fun {n 12} : number
;   {+ n 3}})
   
;(type? '{with {y : number}
;   {+ x y}})

;----------------------------

;Invalid programs the parser will like, but the type
;checker shouldn't:

;(type? '{* 3 {< 7 8}})


;(type? '{with {double : (number -> number) {fun {n : number} {* 2 n}}}
;  {with {x : number 7}
;    {if {= x 10} x double}
; }}
;)

;(type? '{if 5 3 4})


;(type? '{with {double : (number -> number) {fun {n : number} {* 2 n}}}
;  {with {val : boolean {< 8 7}}
;    {doule val}}})
    
    
;(type? '{with {f : number 10}
;  {f 3}
;})


;(type? '{with {b : boolean {= 0 0}}
;    {/ b 45} }
;  )
  
;(type? '{with {b : boolean 5}
;    b })

;(type? '{with {not : (boolean -> boolean)
;         {fun {v : boolean}
;           {if v {= 0 1} {= 0 0}}
;      }  }
;   {not 11}
;})
(type? '(< (- (+ 3 4) 4) 3)) ; return ok bool type
(type? '(+ (- (< 3 4) 4) 3)) ; return get-type error first arguement of sub must be number
