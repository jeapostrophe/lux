#lang racket/base
(require racket/match
         racket/class
         racket/contract/base
         racket/gui/base
         racket/async-channel
         lux/chaos)

(struct gui (depth-box event-ch drawer frame refresh!)
        #:methods gen:chaos
        [(define (chaos-yield c e)
           (yield e))         
         (define (chaos-event c)
           (gui-event-ch c))
         (define (chaos-output! c o)
           (set-box! (gui-drawer c) o)
           ((gui-refresh! c)))
         (define (chaos-label! c l)
           (send (gui-frame c) set-label l))
         (define (chaos-swap! c t)
           (define db (gui-depth-box c))
           (define og (unbox db))
           (set-box! db (add1 og))
           (begin0 (t)
             (if (zero? og)
                 (send (gui-frame c) show #f)
                 (set-box! db og))))])

(define (make-gui #:mode [mode 'draw]
                  #:width [init-w 800]
                  #:height [init-h 600])
  (define events-ch (make-async-channel))
  (define gframe%
    (class frame%
      (define/override (on-size w h)
        (refresh!))
      (define/augment (on-close)
        (async-channel-put events-ch 'close))
      (define/override (on-subwindow-char w ke)
        (async-channel-put events-ch ke))
      (define/override (on-subwindow-event w me)
        (async-channel-put events-ch me))
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

  (send f center)
  (send f show #t)

  (define depth-box (box 0))

  (gui depth-box events-ch drawer f refresh!))

(provide
 (contract-out
  [make-gui
   (->* ()
        (#:mode
         (or/c (one-of/c 'draw 'compat-gl 'core-gl)
               (is-a?/c gl-config%))
         #:width
         exact-nonnegative-integer?
         #:height
         exact-nonnegative-integer?)
        chaos?)]))
