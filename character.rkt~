#lang racket

(require "dice.rkt")
(provide character%)

; state --> options
;  engaged:  attack, parry, disengage, switch weapon, go prone
;  disengaged > 10 away:  missile, close while dodging, close without dodging, attempt escape, go prone
;  disengaged > 5 away:  missile, close while dodging, close without dodging, retreat, retreat while dodging, go prone
;  disengaged < 5  away:  missile, close while dodging, close without dodging, retreat, retreat while dodging, engage, go prone;
;  prone:  stand

(define attribute% (class object%
                     (init name_ value_)
                     (define my-name name_)
                     (define my-base-value value_)
                     (define my-current-value value_)
                     (define/public (get-name) my-name)
                     (define/public (get-base-value) my-base-value)
                     (define/public (get-current-value) my-current-value)

                     (define/public (affect n) (set! my-current-value (my-current-value . + . n)))
                     (define/public (reset) (set! my-current-value my-base-value)
                     
                     )))


(define character%
  (class object%
    (init name_ st_ dx_)
    (define my-name name_)
    (define st st_)
    (define/public (get-st) st)
    (define dx dx_)
    (define subjecttodxadjustment #f)
    (define/public (get-dx) dx)
    (define my-to-hit-dice (new dice% [n_ 3]))

    (define damage-dice-vector (vector
                                '(0 . 0) ; 0
                                '(1 . -5) ; 1
                                '(1 . -4) ; 2
                                '(1 . -3) ; 3
                                '(1 . -2) ; 4
                                '(1 . -2) ; 5
                                '(1 . -1) ; 6
                                '(1 .  0) ; 7
                                '(1 . +1) ; 8
                                '(1 . +2) ; 9
                                '(2 . -2) ; 10
                                '(2 . -1) ; 11
                                '(2 .  0) ; 12
                                '(2 . +1) ; 13
                                '(3 . -1) ; 14
                                '(3 .  0) ; 15
                                ))
                                
                                
    (define (get-damage-dice)
      (let* (
             [ st0 (min (max 0 st) 15)]
             [ q0 (vector-ref damage-dice-vector st0) ]
             )
        (new dicenadds% [n__ (car q0)] [adds_ (cdr q0)])
        ))
             
    
    (super-new)

    (define/public (on-hit n)
      ; if n > 4, -2 dx next turn
      (when (4 . < . n) (set! subjecttodxadjustment #t))
      ; if n > 7   skip next turn
      (set! st (- st n))
      )
    
    
    (define/public (show-me)
      (format "~a:\n==\t==\nst\tdx\n~a\t~a~a\n\n" my-name st dx (if subjecttodxadjustment "*" ""))
      ) ; end show-me
    
    ; hit
    (define/public (hits? [trace-me #f])
      (let* (
            [the-roll (send my-to-hit-dice roll-sum trace-me) ]
            [ adjdx (if subjecttodxadjustment (dx . - . 2 ) dx) ]
            [ result         (cond
                               [ (the-roll . < . 6) #t ]
                               [ (15 . < . the-roll) #f ]
                               [ (the-roll . <= . adjdx) #t ]
                               [ else #f ]
            )]
            ) ;
        (when trace-me
          (let ( [the_name (if subjecttodxadjustment (format "~a*" my-name) my-name)]
                 [the_outcome (if result "   hit!!" "missed..")]
                 )
            (println (format "~a ~a" the_name the_outcome))
;          (if result (println (format "~a    hit!!" my-name)) (println (format "~a missed.." my-name)))
          )) ; end when trace-me
        (set!  subjecttodxadjustment #f) ; dx adjustment never lasts more than one turn.
        result
))
    
    (define/public (get-damage [trace-me #f])
      (if (hits? trace-me)
          (let* ( [d0  (get-damage-dice)]
                  [x0 (send d0 roll-sum trace-me) ]
                  )
            (max 0 x0) ; you can't do negative damage.
            ); if hits
          0 ; else 0
          ))
            
      
    
    ) ; end class
  ) ; end character%
