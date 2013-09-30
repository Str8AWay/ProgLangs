#lang scheme

(define differences
    (lambda (s)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; params:  s - list that differnces          ;
        ;              will be calculated for        ;
        ; returns: a list containing the differences ;
        ;          between adjancent numbers in s    ;
        ;                                            ;
        ; written by Jason Laqua                     ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; cases: when s is less than length 2 return an empty list      ;
        ;        otherwise return the difference between the 1st        ;
        ;                  and 2nd items in list consed with the        ;
        ;                  list returned by differneces of the cdr of s ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (cond [(< (length s) 2) '()]
              [#t (cons (- (cadr s) (car s)) (differences (cdr s)))]
        )
    )
)

(define zip
    (lambda (a b)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; params:  a - first list                        ;
        ;          b - second list                       ;
        ; returns: a list containing pairs consisting    ; 
        ;          of (nth member of a, nth member of b) ;
        ;                                                ;
        ; written by Jason Laqua                         ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; cases: when a and b are null return an empty list ;
        ;        when a is shorter than b return an error   ;
        ;                         'first list too short'    ;
        ;        when b is shorter than b return an error   ;
        ;                         'second list too short'   ;
        ;        otherwise return a list with first element ;
        ;                         of a and b paired         ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (cond [(and (null? a) (null? b)) '()]
              [(null? a) (error 'zip "first list is too short")]
              [(null? b) (error 'zip "second list is too short")]
              [#t (cons (cons (car a) (list (car b))) (zip (cdr a) (cdr b)))]
        )
    )
)

(define deep-add
    (lambda (s)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; params:  s - list to be summed       ;
        ; returns: the sum of any numbers in s ;
        ;                                      ;
        ; written by Jason Laqua               ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; cases: when s is null return 0                          ;
        ;        when (car s) is a number return the addition of  ;
        ;                (car s) and the deep-add of the cdr of s ;
        ;        otherwise return the deep-add of the cdr of s    ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (cond [(null? s) 0]
              [(number? (car s)) (+ (car s) (deep-add (cdr s)))]
              [(list? (car s)) (+ (deep-add (car s)) (deep-add (cdr s)))]
              [#t (deep-add (cdr s))]
        )
    )
)

(define drop-parens
    (lambda (s)
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; params:  s - list to be flattened       ;
        ; returns: a list that has parens removed ;
        ;                                         ;
        ; written by Jason Laqua                  ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ; cases: if s is null return an empty list                   ;
        ;        if s the car of s is a list return the appension of ;
        ;                 drop-parens on the car of s and            ;
        ;                 drop-parens on the cdr of s                ;
        ;        otherwise return the cons of the car of s and       ;
        ;                 drop-parens on the cdr of s                ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (cond [(null? s) '()]
              [(list? (car s)) (append (drop-parens (car s)) (drop-parens (cdr s)))]
              [#t (cons (car s) (drop-parens (cdr s)))]
        )
    )
)

'TestCases
'differences
'(differences '(2 7 10 3 6))
(differences '(2 7 10 3 6)) ; (5 3 -7 3)
'(differences '(1 3))
(differences '(1 3)) ; (2)
'(differences '())
(differences '()) ; ()
'(differences '(1 2 -3 -4 7 12))
(differences '(1 2 -3 -4 7 12)) ; (1 -5 -1 11 5)
'(differences '(1))
(differences '(1)) ; ()
'(differences '(1 1 1 1))
(differences '(1 1 1 1)) ; (0 0 0)

'zip
'(zip '(a b c d e) '(32 7 10 3 1) )
(zip '(a b c d e) '(32 7 10 3 1) ) ; ((a 32) (b 7) (c 10) (d 3) (e 1))
'(zip '() '() )
(zip '() '() ) ; ()
'(zip '((a b) c (d e f)) '(c (a) (b c)) )
(zip '((a b) c (d e f)) '(c (a) (b c)) ) ; (((a b) c) (c (a)) ((d e f) (b c)))
'(zip '(1 2 -3) '(50 40 30 20))
;(zip '(1 2 -3) '(50 40 30 20)) ; x x zip: first list too short
'(zip '(1 2 3 4) '(1 2 3))
;(zip '(1 2 3 4) '(1 2 3)) ; x x zip: second list too short
'(zip '((a (b (c))) (d (e (f)))) '((1 (2 (3 3))) (4 5 6)))
(zip '((a (b (c))) (d (e (f)))) '((1 (2 (3 3))) (4 5 6))) ; (((a (b (c))) (1 (2 (3 3)))) ((d (e (f))) (4 5 6)))

'deep-add
'(deep-add '(5 a b 8 2))
(deep-add '(5 a b 8 2)) ; 15
'(deep-add '((4 (6 1)) 2 3 (4)))
(deep-add '((4 (6 1)) 2 3 (4))) ; 25
'(deep-add '(these (aren't 77) (all 32 (numbers 93 here))))
(deep-add '(these (aren't 77) (all 32 (numbers 93 here)))) ; 202
'(deep-add '())
(deep-add '()) ; 0
'(deep-add '(no numbers here))
(deep-add '(no numbers here)) ; 0
'(deep-add '((((5)))))
(deep-add '((((5))))) ; 5
'(deep-add '((this (is an (endless pit of 0)) (3)) 5))
(deep-add '((this (is an (endless pit of 0)) (3)) 5)) ; 8

'drop-parens
'(drop-parens '((a 34)(b 77)(g 6)) )
(drop-parens '((a 34)(b 77)(g 6)) ) ; (a 34 b 77 g 6)
'(drop-parens '(a b c) )
(drop-parens '(a b c) ) ; (a b c)
'(drop-parens '() )
(drop-parens '() ) ; ()
'(drop-parens '(()((() x)())) )
(drop-parens '(()((() x)())) ) ; (x)
'(drop-parens '(1 2 (3 4) 5 (6 (7 (8) 9))))
(drop-parens '(1 2 (3 4) 5 (6 (7 (8) 9)))) ; (1 2 3 4 5 6 7 8 9)
'(drop-parens '(()))
(drop-parens '(())) ; ()
