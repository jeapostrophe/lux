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

(struct spin (layer color frame x y)
        #:methods gen:word
        [(define (word-fps w)
           60.0)
         (define (word-label s ft)
           (lux-standard-label "Spin!" ft))
         (define (word-event w e)
           (match-define (spin layer color f x y) w)
           (cond
            [(or (eq? e 'close)
                 (and (key-event? e)
                      (eq? (send e get-key-code) 'escape)))
             #f]
            [(and (key-event? e)
                  (eq? (send e get-key-code) #\space))
             (spin layer (fxmodulo (fx+ 1 color) (length COLORS)) f x y)]
            [(mouse-event? e)
             (spin layer color f
                   (send e get-x) 
                   (send e get-y))]
            [(and (key-event? e)
                  (eq? (send e get-key-code) #\return))
             (spin-it (add1 layer))
             w]
            [else
             w]))
         (define (word-output w)
           (match-define (spin layer color f x y) w)
           (lambda (width height dc)
             (send dc set-background (list-ref COLORS color))
             (send dc clear)
             (send dc set-rotation (* (/ f 360) 2 3.14))
             (send dc set-origin x y)
             (send dc draw-text (format "~a: Spinning!" layer) 0 0)))
         (define (word-tick w)
           (match-define (spin layer color f x y) w)
           (spin layer color (fxmodulo (fx+ f 1) 360) x y))])

(define (spin-it layer)
  (define s
    (spin layer 0 0 0 0))
  (fiat-lux s))

(module+ main
  (call-with-chaos
   (make-gui)
   (λ ()
     (spin-it 0))))
