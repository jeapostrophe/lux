#lang racket/base
(require racket/class
         racket/draw
         racket/fixnum
         racket/contract/base
         pict
         pict/convert)

(define (make-gui/val #:scale? [scale? #t])
  (λ (o)
    (define p (pict-convert o))
    (λ (w h dc)
      (parameterize ([dc-for-text-size dc])
        (send dc set-background "black")
        (send dc clear)
        (define sp
          (if scale?
              (scale-to-fit p w h)
              p))
        (define spw (pict-width sp))
        (define left (/ (- w spw) 2))
        (define sph (pict-height sp))
        (define top (/ (- h sph) 2))
        (send dc set-brush "white" 'solid)
        (send dc draw-rectangle left top spw sph)
        (draw-pict sp dc left top)))))

(provide
 (contract-out
  [make-gui/val
   (->* () (#:scale? boolean?)
        (-> pict-convertible?
            (-> real? real? (is-a?/c dc<%>)
                any)))]))
