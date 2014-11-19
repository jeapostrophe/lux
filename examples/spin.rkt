#lang racket/base
(require racket/match
         racket/fixnum
         racket/draw
         racket/gui/base
         racket/class
         lux
         lux/chaos/gui)

(define COLORS
  '("red" "orange" "yellow" "green" "blue" "indigo" "violet"))

(struct spin (color frame x y)
        #:methods gen:word
        [(define (word-label s ft)
           (lux-standard-label "Spin!" ft))
         (define (word-tick w es)
           (match-define (spin color f x y) w)
           (define closed? #f)
           (for ([e es])
             (match e
               ['close
                (set! closed? #t)]
               [(? (λ (x) (is-a? x mouse-event%)) me)
                (set! x (send me get-x))
                (set! y (send me get-y))]
               [(? (λ (x) (is-a? x key-event%)) ke)
                (set! color (fxmodulo (fx+ 1 color) (length COLORS)))]))
           (match closed?
             [#t
              (values #f w)]
             [#f
              (values (spin color (fxmodulo (fx+ f 1) 360) x y)
                      (lambda (width height dc)
                        (set! x (fxmin width (fxmax x 0)))
                        (set! y (fxmin height (fxmax y 0)))
                        (send dc set-background (list-ref COLORS color))
                        (send dc clear)
                        (send dc set-rotation (* (/ f 360) 2 3.14))
                        (send dc set-origin x y)
                        (send dc draw-text "Spinning!" 0 0)))]))])

(module+ main
  (call-with-chaos
   (make-gui 60.0)
   (λ () (fiat-lux (spin 0 0 400 300)))))
