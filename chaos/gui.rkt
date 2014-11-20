#lang racket/base
(require racket/match
         racket/class
         racket/contract/base
         racket/gui/base
         data/queue
         lux/chaos)

(struct *sbox (sema box))
(define (sbox v)
  (*sbox (make-semaphore 1) (box v)))
(define (sbox-swap! sb new)
  (match-define (*sbox sema b) sb)
  (call-with-semaphore sema
    (λ ()
      (begin0 (unbox b)
        (set-box! b new)))))
(define (sbox-poke sb f)
  (match-define (*sbox sema b) sb)
  (call-with-semaphore sema
    (λ () (f (unbox b)))))

(struct gui (depth-box events-sbox fps drawer frame refresh!)
        #:methods gen:chaos
        [(define (chaos-fps c)
           (gui-fps c))
         (define (chaos-yield c e)
           (yield e))
         (define (chaos-inputs c)
           (define eb (gui-events-sbox c))
           (define new-q (make-queue))
           (define q (sbox-swap! eb new-q))
           (in-queue q))
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

(define (make-gui fps
                  #:mode [mode 'draw]
                  #:width [init-w 800]
                  #:height [init-h 600])
  (define events-box (sbox (make-queue)))
  (define gframe%
    (class frame%
      (define/override (on-size w h)
        (refresh!))
      (define/augment (on-close)
        (sbox-poke events-box (λ (q) (enqueue! q 'close))))
      (define/override (on-subwindow-char w ke)
        (sbox-poke events-box (λ (q) (enqueue! q ke))))
      (define/override (on-subwindow-event w me)
        (sbox-poke events-box (λ (q) (enqueue! q me))))
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

  (gui depth-box events-box fps drawer f refresh!))

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
