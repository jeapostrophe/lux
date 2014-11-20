#lang racket/base
(require racket/match
         racket/fixnum
         racket/draw
         racket/class
         lux
         lux/chaos/gui
         lux/chaos/gui/key
         lux/chaos/gui/mouse)

(define COLORS
  '("red" "orange" "yellow" "green" "blue" "indigo" "violet"))

(struct spin (layer ks ms color frame)
        #:methods gen:word
        [(define (word-label s ft)
           (lux-standard-label "Spin!" ft))
         (define (word-event w e)
           ;; xxx remove mutation
           (match-define (spin layer ks ms color f) w)
           (define closed? #f)
           (match e
             ['close
              (set! closed? #t)]
             [(? mouse-event? me)
              (mouse-state-update! ms me)]
             [(? key-event? ke)
              (key-state-update! ks ke)])
           (when (key-state-set?! ks #\space)
             (set! color (fxmodulo (fx+ 1 color) (length COLORS))))
           (when (key-state-set?! ks #\return)
             (spin-it! (add1 layer)))
           (match (or closed?
                      (key-state-set?! ks 'escape))
             [#t
              #f]
             [#f
              (spin layer ks ms color f)]))
         (define (word-output w)
           (match-define (spin layer ks ms color f) w)
           (define x (mouse-state-x ms))
           (define y (mouse-state-y ms))
           (lambda (width height dc)
             (send dc set-background (list-ref COLORS color))
             (send dc clear)
             (send dc set-rotation (* (/ f 360) 2 3.14))
             (send dc set-origin x y)
             (send dc draw-text (format "~a: Spinning!" layer) 0 0)))
         (define (word-tick w)
           (match-define (spin layer ks ms color f) w)
           (spin layer ks ms color (fxmodulo (fx+ f 1) 360)))])

(define (spin-it! layer)
  (define s
    (spin layer
          (make-key-state)
          (make-mouse-state)
          0 0))
  (fiat-lux s))

(module+ main
  (call-with-chaos
   (make-gui 60.0)
   (λ ()
     (spin-it! 0))))
