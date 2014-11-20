#lang racket/base
(require racket/match
         racket/fixnum
         racket/draw
         racket/gui/base
         racket/class
         lux
         lux/chaos/gui
         lux/chaos/gui/mouse)

(define COLORS
  '("red" "orange" "yellow" "green" "blue" "indigo" "violet"))

(struct spin (ms color frame)
        #:methods gen:word
        [(define (word-label s ft)
           (lux-standard-label "Spin!" ft))
         (define (word-tick w es)
           (match-define (spin ms color f) w)
           (define closed? #f)
           (for ([e es])
             (match e
               ['close
                (set! closed? #t)]
               [(? mouse-event? me)
                (mouse-state-update! ms me)]
               [(? (λ (x) (is-a? x key-event%)) ke)
                (set! color (fxmodulo (fx+ 1 color) (length COLORS)))]))
           (define x (mouse-state-x ms))
           (define y (mouse-state-y ms))
           (match closed?
             [#t
              (values #f w)]
             [#f
              (values (spin ms color (fxmodulo (fx+ f 1) 360))
                      (lambda (width height dc)
                        (send dc set-background (list-ref COLORS color))
                        (send dc clear)
                        (send dc set-rotation (* (/ f 360) 2 3.14))
                        (send dc set-origin x y)
                        (send dc draw-text "Spinning!" 0 0)))]))])

(module+ main
  (call-with-chaos
   (make-gui 60.0)
   (λ () (fiat-lux (spin (make-mouse-state) 0 0)))))
