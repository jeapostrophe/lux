#lang racket/base
(require racket/match
         racket/class
         racket/contract/base
         racket/gui/base
         data/queue
         lux/chaos)

(struct gui (events-box fps drawer frame refresh!)
        #:methods gen:chaos
        [(define (chaos-fps c)
           (gui-fps c))
         (define (chaos-yield c e)
           (yield e))
         (define (chaos-inputs c)
           (define eb (gui-events-box c))
           (define new-q (make-queue))
           (define q (unbox eb))
           (set-box! eb new-q)
           (in-queue q))
         (define (chaos-output! c o)
           (set-box! (gui-drawer c) o)
           ((gui-refresh! c)))
         (define (chaos-label! c l)
           (send (gui-frame c) set-label l))])

(define (make-gui fps
                  #:mode [mode 'draw]
                  #:width [init-w 800]
                  #:height [init-h 600])
  (define events-box (box (make-queue)))
  (define gframe%
    (class frame%
      (define/override (on-size w h)
        (refresh!))
      (define/augment (on-close)
        (enqueue! (unbox events-box) 'close))
      (define/override (on-subwindow-char w ke)
        (enqueue! (unbox events-box) ke))
      (define/override (on-subwindow-event w me)
        (enqueue! (unbox events-box) me))
      (super-new)))

  (define drawer (box void))
  (define (paint-canvas c dc)
    ((unbox drawer)
     (send c get-width)
     (send c get-height)
     dc))

  (define f
    (new gframe%
         [label ""]
         [width init-w]
         [height init-h]
         [style '(fullscreen-button)]))

  (define gl-config
    (match mode
      ['draw #f]
      ['compat-gl
       (new gl-config%)]
      ['core-gl
       (define gl-config (new gl-config%))
       (send gl-config set-legacy? #f)]
      [gl-config
       gl-config]))

  (define c
    (new canvas% [parent f]
         [paint-callback paint-canvas]
         [style
          (cons 'no-autoclear
                (if gl-config '(gl) '()))]))
  (define (refresh!)
    (send c refresh))

  (send f show #t)

  (gui events-box fps drawer f refresh!))

(provide
 (contract-out
  [make-gui
   (->* (flonum?)
        (#:mode
         (or/c (one-of/c 'draw 'compat-gl 'core-gl)
               (is-a?/c gl-config%))
         #:width
         exact-nonnegative-integer?
         #:height
         exact-nonnegative-integer?)
        chaos?)]))
