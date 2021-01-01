#lang racket

(require "dice.rkt")
(require "character.rkt")

(define-syntax-rule (roll-dice n)
  ( let (
         [d0 (new dice% [n_ n])]
         )
     (send d0 roll-sum)))

(define b0 (new character% [name_ "b0"] [st_ (roll-dice 3) ] [dx_ (roll-dice 3) ]))


(define-syntax-rule (show! it) (display (send it show-me)))
(define-syntax-rule (on-hit! it n)  (send it on-hit n))
(define-syntax-rule (dam! it)  (send it get-damage))
(define-syntax-rule (damt! it) (send it get-damage #t))
;(show! b0)

(define a0 (new character% [name_ "a0"] [st_ 13] [dx_ 11])) ; exp 34

;(show! a0)

(define-syntax-rule (turn!) (begin (show! a0) (show! b0)) )
; (turn!)

(define-syntax-rule (strike! itsource ittarget) (on-hit! ittarget (dam! itsource)))
(define-syntax-rule (striket! itsource ittarget) (on-hit! ittarget (damt! itsource)))

(define-syntax-rule (melee-round! itsource ittarget)
  (begin
    (strike! itsource ittarget)
    (strike! itsource ittarget)
    (turn!)
    ))

(define-syntax-rule  (plusminusone) ((2 . * . (random 2)) . - . 1))

(define-syntax-rule (melee-roundt! itsource ittarget)
  (let* (
        [source-dx (send itsource get-dx)]
        [target-dx_ (send ittarget get-dx)]
        [target-dx (if (source-dx . eq? . target-dx_)
                       (target-dx_ . + . (plusminusone)) target-dx_)]
        )
  (begin
    (when (target-dx . < . source-dx ) (striket!  itsource ittarget))
    (striket! ittarget itsource )
    (when (source-dx . < . target-dx ) (striket!  itsource ittarget))
    (turn!)
    )))


(define (get-character-action) 93)

(define-syntax-rule (go!) (begin
                   (turn!)
                   (let ([x (read)])
                     (println x)
                     
                     )
                   (melee-roundt! a0 b0)
                   ))