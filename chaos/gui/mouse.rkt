#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/class
         racket/gui/base
         racket/generic
         racket/contract/base
         data/queue
         lux/chaos)

(struct mouse-state
  (_
   x y
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

(struct gui/mouse (ms g)
        #:methods gen:chaos
        [(define/generic super-fps chaos-fps)
         (define/generic super-yield chaos-yield)
         (define/generic super-inputs chaos-inputs)
         (define/generic super-output! chaos-output!)
         (define/generic super-label! chaos-label!)
         (define/generic super-swap! chaos-swap!)
         (define (chaos-fps c)
           (super-fps (gui/mouse-g c)))
         (define (chaos-yield c e)
           (super-yield (gui/mouse-g c) e))
         (define (chaos-inputs c)
           (define ms (gui/mouse-ms c))
           (define inner (super-inputs (gui/mouse-g c)))
           (define q (make-queue))
           (for ([e inner])
             (match e
               [(? (λ (x) (is-a? x mouse-event%)) me)
                (set-mouse-state-x! ms (send me get-x))
                (set-mouse-state-y! ms (send me get-y))
                (set-mouse-states
                 ms me
                 (left right middle shift control meta alt mod3 mod4 mod5))]
               [_
                (enqueue! q e)]))
           (enqueue! q ms)
           (in-queue q))
         (define (chaos-output! c o)
           (super-output! (gui/mouse-g c) o))
         (define (chaos-label! c l)
           (super-label! (gui/mouse-g c) l))
         (define (chaos-swap! c t)
           (super-swap! (gui/mouse-g c) t))])

(define (make-gui/mouse g)
  (define ms
    (mouse-state (gensym) 0 0 #f #f #f #f #f #f #f #f #f #f))
  (gui/mouse ms g))
(provide
 (contract-out
  [struct mouse-state
    ([_ symbol?]
     [x exact-integer?]
     [y exact-integer?]
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
  [make-gui/mouse
   (-> chaos?
       chaos?)]))
