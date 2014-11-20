#lang racket/base
(require racket/class
         racket/generic
         racket/contract/base
         pict
         pict/convert
         lux/chaos)

(struct gui/val (scale? g [last #:mutable])
        #:methods gen:chaos
        [(define/generic super-fps chaos-fps)
         (define/generic super-yield chaos-yield)
         (define/generic super-event chaos-event)
         (define/generic super-output! chaos-output!)
         (define/generic super-label! chaos-label!)
         (define/generic super-swap! chaos-swap!)
         (define (chaos-fps c)
           (super-fps (gui/val-g c)))
         (define (chaos-yield c e)
           (super-yield (gui/val-g c) e))
         (define (chaos-event c)
           (super-event (gui/val-g c)))
         ;; xxx change this to be a helper for word's output creation
         (define (chaos-output! c o)
           (unless (eq? o (gui/val-last c))
             (set-gui/val-last! c o)
             (define p (pict-convert o))
             (super-output!
              (gui/val-g c)
              (λ (w h dc)
                (parameterize ([dc-for-text-size dc])
                  (send dc set-background "black")
                  (send dc clear)
                  (define sp
                    (if (gui/val-scale? c)
                        (scale-to-fit p w h)
                        p))
                  (define spw (pict-width sp))
                  (define left (/ (- w spw) 2))
                  (define sph (pict-height sp))
                  (define top (/ (- h sph) 2))
                  (send dc set-brush "white" 'solid)
                  (send dc draw-rectangle left top spw sph)
                  (draw-pict sp dc left top))))))
         (define (chaos-label! c l)
           (super-label! (gui/val-g c) l))
         (define (chaos-swap! c t)
           (super-swap! (gui/val-g c) t))])

(define (make-gui/value g #:scale? [scale? #t])
  (gui/val scale? g #f))
(provide
 (contract-out
  [make-gui/value
   (->* (chaos?)
        (#:scale? boolean?)
        chaos?)]))
