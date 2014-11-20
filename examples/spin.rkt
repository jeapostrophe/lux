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

(struct spin (ks ms color frame)
        #:methods gen:word
        [(define (word-label s ft)
           (lux-standard-label "Spin!" ft))
         (define (word-tick w es)
           (match-define (spin ks ms color f) w)
           (define closed? #f)
           (for ([e es])
             (match e
               ['close
                (set! closed? #t)]
               [(? mouse-event? me)
                (mouse-state-update! ms me)]
               [(? key-event? ke)
                (key-state-update! ks ke)]))
           (define x (mouse-state-x ms))
           (define y (mouse-state-y ms))
           (when (key-state-set? ks #\space)
             (set! color (fxmodulo (fx+ 1 color) (length COLORS))))
           (match closed?
             [#t
              (values #f w)]
             [#f
              (values (spin ks ms color (fxmodulo (fx+ f 1) 360))
                      (lambda (width height dc)
                        (send dc set-background (list-ref COLORS color))
                        (send dc clear)
                        (send dc set-rotation (* (/ f 360) 2 3.14))
                        (send dc set-origin x y)
                        (send dc draw-text "Spinning!" 0 0)))]))])

(module+ main
  (define s 
    (spin (make-key-state)
          (make-mouse-state)
          0 0))
  (call-with-chaos
   (make-gui 60.0)
   (λ () (fiat-lux s))))
