#lang racket

(require "dice.rkt")
(provide attribute%)



(define nonnegative/c (and/c number? (or/c positive? zero?)))

(define/contract attribute%
  (class/c            [get-current-value (->m nonnegative/c )])

 (class object%
                    (init name_ value_)
   (super-new)
                     (define my-name name_)
                     (define my-base-value value_)
                     (define my-current-value value_)
                     (define/public (get-name) my-name)
                     (define/public (get-base-value) my-base-value)
                     (define/public (get-current-value) my-current-value)

                     (define/public (affect n)
                       (let (
                             [newval (max (my-current-value . + . n) 0)] ; don't go below zero
                             )
                       (set! my-current-value newval)
                       ))
                       
                     (define/public (reset) (set! my-current-value my-base-value)
                     
                     )))

