#lang scheme

[define atom?
  [lambda [x]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; params:  x - item             ;
    ; returns: if x is an atom      ;
    ;                               ;
    ; written by The Little Schemer ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    [and [not [pair? x]] [not [null? x]]]
  ]
]

(define beginning?
  (lambda (s t)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; params:  s - pattern list                        ;
    ;          t - list to check                       ;
    ; returns: true if s is at the beginning of t      ;
    ;          false if s is not at the beginning of t ;
    ;                                                  ;
    ; written by Jason Laqua                           ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (cond ((null? s) #t)
          ((null? t) #f)
          ((equal? (car s) (car t)) (beginning? (cdr s) (cdr t)))
          (#t #f)
    )
  )
)

[define sublist?
  [lambda [s t]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; params:  s - pattern list           ;
    ;          t - list to check          ;
    ; returns: true if s is within t      ;
    ;          false if s is not within t ;
    ;                                     ;
    ; written by Jason Laqua              ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    [cond [[null? t] #f]
          [[beginning? s t] [beginning? s t]]
          [#t [sublist? s [cdr t]]]
    ]
  ]
]

[define deepcount
  [lambda [e s]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; params:  e - element to count    ;
    ;          s - list to be searched ;
    ; returns: occurances of e in s    ;
    ;                                  ;
    ; written by Jason Laqua           ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    [cond [[null? s] 0]
          [[atom? [car s]] [cond [[equal? e [car s]] [+ 1 [deepcount e [cdr s]]]]
                                 [#t [deepcount e [cdr s]]]]]
          [#t [+ [deepcount e [car s]] [deepcount e [cdr s]]]]
    ]
  ]
]

'"beginning? test cases"
(beginning? '(a b c) '(a b c d e))                                      ; #t
(beginning? '(a b c) '(a b))                                            ; #f
(beginning? '(ant box cow) '(ant box cow))                              ; #t
(beginning? '() '(1 2 3))                                               ; #t
(beginning? '(this is nested) '(this is (nested)))                      ; #f
[beginning? '[is this at the beginning] '[this is at the beginning]]    ; #f
[beginning? '[the [quick brown]] '[the [quick brown] fox jumped]]       ; #t

'"sublist? test cases"
(sublist? '(c a b) '(a b a c a))                                ; #f
(sublist? '(green red green) '(green purple green red green))   ; #t
(sublist? '(1 2 3) '())                                         ; #f
(sublist? '(a b) '(d e (a b) c))                                ; #f
(sublist? '((4 5) 6) '(4 3 7 (4 5) 6 8))                        ; #t
[sublist? '[a [b [c [d [e]]]]] '[bad a [b [c [d [e]]]] alpha]]  ; #t
[sublist? '[qwerty keyboard] '[dvorak keyboard qwerty qwertz]]  ; #f

'"deepcount test cases"
(deepcount 'a '(a b c a d c))                                                   ; 2 
(deepcount 'a '(5 4 3 2))                                                       ; 0
(deepcount 'apple '((5 3 (apple ball)) apple (x (y (5 7) apple) ball apple)))   ; 4
(deepcount 'x '(hex xx (y x z x) box x ray))                                    ; 3
[deepcount 'foo '[foo bar [foo bar [foo bar] baz spam eggs] foo blah]]          ; 4
[deepcount 2 '[2 3 4 3 2 4 3 2 [2 [2 [2 [2 [2 [2]]]] 2] 45 NA] 3 4 [3 2] 2]]    ; 12
