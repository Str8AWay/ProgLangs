#lang racket

[define atom?
  [lambda [x]
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