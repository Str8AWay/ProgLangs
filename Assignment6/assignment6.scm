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
  [IF (test JOE+?) (tval JOE+?) (fval JOE+?)]
  [id (name symbol?)]
  [fun (arg-name symbol?) (body JOE+?)]
  [refun (bound-id symbol?) (bound-body JOE+?)]
  [app (fun-expr JOE+?) (arg JOE+?)]
  [SET (var symbol?) (value JOE+?)]
  [seqn (elements list?)])

(define-type JOE+-value
  [numV (n number?)]
  [booleanV (b boolean?)]
  [closureV (param symbol?)
            (body JOE+?)
            (env Env?)]
  [refclosV (param symbol?)
            (body JOE+?)
            (env Env?)]
  [boxV (location number?)]
)

(define (boxed-JOE+-value? b)
  (and (box? b)
       (JOE+-value? (unbox b))
  )
)

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (location number?) 
        (env Env?)]
  [aRecSub (name symbol?)
          (value boxed-JOE+-value?)
          (env Env?)]
)

(define-type Store
  [mtSto]
  [aSto (location number?)
        (value JOE+-value?)
        (store Store?)]
)

(define-type Value*Store
  [v*s (value JOE+-value?) (store Store?)]
)

(define (cyclically-bind-and-interp bound-id named-expr env)
  (let* ((value-holder (box (numV 55)))
         (new-env (aRecSub bound-id value-holder env))
         (named-expr-val (interp named-expr new-env)))
     (set-box! value-holder named-expr-val)
     new-env
  ))

(define next-location
  (local ([define last-loc (box -1)])
    (lambda (store)
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))


;; env-lookup: symbol Env --> JOE+
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding found for id ~a" name)]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? name bound-name)
              bound-location
              (env-lookup name rest-env))]
    [aRecSub (bound-name boxed-value rest-env)
          (if (symbol=? name bound-name)
              (unbox boxed-value)
              (env-lookup name rest-env))]
 ))

;; store-lookup : location Store → JOE+-value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= location loc-index)
              value
              (store-lookup loc-index rest-store))]
  )
)

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
(define (interp expr env store)
  (type-case JOE+ expr
    [num (n) (v*s (numV n) store)]
    [add (l r) 
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (tree-add l-value r-value)
                            r-store)
                  ]
                )
           ]
         )]
    [sub (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (tree-sub l-value r-value)
                            r-store)
                  ]
                )
           ]
         )]
    [mult (l r) 
          (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (tree-mult l-value r-value)
                            r-store)
                  ]
                )
           ]
         )]
    [div (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (tree-div l-value r-value)
                            r-store)
                  ]
                )
           ]
         )]
    [minus (oper) 
           (type-case Value*Store (interp oper env store)
             [v*s (new-oper new-store)
                  (v*s (tree-minus (interp new-oper)) 
                       new-store)
             ]
           )]
    [equ (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (tree-equ l-value r-value)
                            r-store)
                  ]
                )
           ]
         )]
    [lt (l r)
        (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (tree-lt l-value r-value)
                            r-store)
                  ]
                )
           ]
         )]
    [IF (test tval fval)
        (type-case Value*Store (interp test env store)
          [v*s (test-value test-store)
             (cond [(booleanV-b test-value) (interp tval env test-store)]
                   [#t (interp fval env test-store)]
             )]
        )]
    [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
    [fun (arg body) (v*s (closureV arg body env) store)]
    [refun (bound-id bound-body) (v*s (refclosV bound-id bound-body env) store)]
    [app (fun-expr arg-expr)
         (type-case Value*Store (interp fun-expr env store)
           [v*s (fun-value fun-store)
                (type-case JOE+-value fun-value
                  [closureV (cl-param cl-body cl-env)            
                            (type-case Value*Store (interp arg-expr env fun-store)
                              [v*s (arg-value arg-store)
                                   (local ([define new-loc (next-location arg-store)])
                                     (interp (closureV-body fun-value)
                                             (aSub cl-param
                                                   new-loc
                                                   cl-env)
                                             (aSto new-loc
                                                   arg-value
                                                   arg-store)))])]
                  [refclosV (cl-param cl-body cl-env)
                            (local ([define arg-loc (env-lookup (id-name arg-expr) env)])
                              (interp cl-body
                                      (aSub cl-param
                                            arg-loc
                                            cl-env)
                                      fun-store))]
                  [numV (_) (error 'interp "trying to apply a number")]
                  [booleanV (_) (error 'interp "trying to apply a boolean")]
                  [boxV (_) (error 'interp "trying to apply a box")])]
           )]
    [SET (var value)
         (type-case Value*Store (interp value env store)
           [v*s (value-value value-store)
                (local ([define the-loc (env-lookup var env)])
                  (v*s value-value
                       (aSto the-loc value-value value-store)))])]
    [seqn (elements)
          (cond [(null? elements) (error 'interp "empty list given to seqn")]
                [(eq? 1 (length elements))  (type-case Value*Store (interp (parse (car elements)) env store)
                                              [v*s (car-value car-store)
                                                   (v*s car-value car-store)])]
                [#t (type-case Value*Store (interp (parse (car elements)) env store)
                      [v*s (car-value car-store)
                           (interp (seqn (cdr elements)) env car-store)])])]
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
        [(eq? (first sexp) 'refun) (cond [(eq? 3 (length sexp)) (cond [(and (list? (second sexp)) (eq? 1 (length (second sexp))) (symbol? (car (second sexp))))
                                                                         (refun (first (second sexp))
                                                                                (parse (third sexp)))]
                                                                      [#t (error 'parse "inccorect first operand of refun")])]
                                         [#t (error 'parse "incorrect number of arguments for refun")])]
        [(eq? (first sexp) 'set) (cond [(eq? 3 (length sexp)) (SET (second sexp) (parse (third sexp)))]
                                       [#t (error 'parse "incorrect number of arguments for set")])]
        [(eq? (first sexp) 'seqn) (seqn (cdr sexp))]
        [(eq? 2 (length sexp)) (app (parse (first sexp)) 
                                   (parse (second sexp)))]
        [else (error 'parse "syntax error")]
     ))

(define (run expr) (interp (parse expr) (mtSub) (mtSto)))


