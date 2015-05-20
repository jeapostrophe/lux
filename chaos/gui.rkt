#lang racket/base
(require racket/match
         racket/class
         racket/contract/base
         racket/gui/base
         racket/async-channel
         lux/chaos)

;; Robby says that I could rework this to remove the event-ch by
;; having capturing the continuation, storing it, and then calling it
;; from within the callback once an event is ready.

(struct gui (event-ch drawer frame refresh!)
        #:methods gen:chaos
        [(define (chaos-yield c e)
           (yield e))
         (define (chaos-event c)
           (gui-event-ch c))
         (define (chaos-output! c o)
           (when o
             (set-box! (gui-drawer c) o))
           ((gui-refresh! c)))
         (define (chaos-label! c l)
           (send (gui-frame c) set-label l))
         (define (chaos-stop! c)
           (send (gui-frame c) show #f))])

(define (make-gui #:mode [mode 'draw]
                  #:opengl-hires? [opengl-hires? #f]
                  #:start-fullscreen? [start-fullscreen? #f]
                  #:icon [icon #f]
                  #:frame-style [frame-style '()]
                  #:width [the-init-w #f]
                  #:height [the-init-h #f])
  (define-values (start-x start-y init-w init-h)
    (cond
      [start-fullscreen?
       (define-values (x y) (get-display-left-top-inset #t))
       (define-values (w h) (get-display-size #t))
       (values (* -1 x) (* -1 y) w h)]
      [else
       (values #f #f 800 600)]))

  ;; xxx start-x/y isn't working
  
  (define events-ch (make-async-channel))
  (define gframe%
    (class frame%
      (define/override (on-size w h)
        (define-values (cw ch) (send c get-scaled-client-size))
        (async-channel-put events-ch (list 'resize cw ch))
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
    (define-values (cw ch) (send c get-scaled-client-size))
    ((unbox drawer) cw ch dc))

  ;; (printf "starting at ~v\n" (vector start-x start-y))

  (define f
    (new gframe%
         [label ""]
         [x start-x]
         [y start-y]
         [width init-w]
         [height init-h]
         [style frame-style]))

  (define gl-config
    (match mode
      ['draw #f]
      ['gl-compat
       (new gl-config%)]
      ['gl-core
       (define gl-config (new gl-config%))
       (send gl-config set-legacy? #f)
       gl-config]
      [gl-config
       gl-config]))

  (when (and gl-config opengl-hires?)
    (send gl-config set-hires-mode #t))

  (define c
    (new canvas% [parent f]
         [paint-callback paint-canvas]
         [gl-config gl-config]
         [style
          (cons 'no-autoclear
                (if gl-config '(gl) '()))]))
  (define the-refresh-sema (make-semaphore 0))
  (define (refresh!)
    (queue-callback 
     (λ ()
       (send c refresh-now)
       (semaphore-post the-refresh-sema))
     #f)
    (yield the-refresh-sema))

  (cond
    [start-fullscreen?
     (send f move start-x start-y)]
    [else
     (send f center)])
  (send f show #t)

  (when icon
    (define icon-bm
      (if (is-a? icon bitmap%)
          icon
          (read-bitmap icon)))
    (when (eq? 'macosx (system-type 'os))
      ((dynamic-require 'drracket/private/dock-icon 'set-dock-tile-bitmap)
       icon-bm)))

  (gui events-ch drawer f refresh!))

(provide
 (contract-out
  [make-gui
   (->* ()
        (#:mode
         (or/c (one-of/c 'draw 'gl-compat 'gl-core)
               (is-a?/c gl-config%))
         #:opengl-hires?
         boolean?
         #:start-fullscreen?
         boolean?
         #:frame-style
         (listof symbol?)
         #:icon
         (or/c #f path-string? (is-a?/c bitmap%))
         #:width
         exact-nonnegative-integer?
         #:height
         exact-nonnegative-integer?)
        chaos?)]))
