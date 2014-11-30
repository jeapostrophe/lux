#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/class
         racket/gui/base
         racket/contract/base)

(struct mouse-state
  (x y
   left? right? middle?
   shift? control? meta? alt?
   mod3? mod4? mod5?)
  #:mutable)

(define-syntax (set-mouse-state stx)
  (syntax-case stx ()
    [(_ ms me id)
     (with-syntax ([set-mouse-state-id?!
                    (format-id #'id "set-mouse-state-~a?!" #'id)]
                   [get-id-down
                    (format-id #'id "get-~a-down" #'id)])
       (syntax/loc stx
         (set-mouse-state-id?! ms (send me get-id-down))))]))
(define-syntax-rule (set-mouse-states ms me (id ...))
  (begin (set-mouse-state ms me id) ...))

(define (mouse-event? x)
  (is-a? x mouse-event%))
(define (mouse-event-xy me)
  (values (send me get-x) (send me get-y)))

(define (mouse-state-update! ms me)
  (set-mouse-state-x! ms (send me get-x))
  (set-mouse-state-y! ms (send me get-y))
  (set-mouse-states
   ms me
   (left right middle shift control meta alt mod3 mod4 mod5)))

(define (make-mouse-state)
  (mouse-state 0 0 #f #f #f #f #f #f #f #f #f #f))
(provide
 (contract-out
  [struct mouse-state
    ([x real?]
     [y real?]
     [left? boolean?]
     [right? boolean?]
     [middle? boolean?]
     [shift? boolean?]
     [control? boolean?]
     [meta? boolean?]
     [alt? boolean?]
     [mod3? boolean?]
     [mod4? boolean?]
     [mod5? boolean?])]
  [mouse-event?
   (-> any/c boolean?)]
  [mouse-event-xy
   (-> mouse-event? (values real? real?))]
  [make-mouse-state
   (-> mouse-state?)]
  [mouse-state-update!
   (-> mouse-state? (is-a?/c mouse-event%)
       any)]))
