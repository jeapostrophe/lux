#lang racket/base
(require racket/match
         racket/fixnum
         racket/gui/base
         racket/class
         (prefix-in pict: pict)
         (prefix-in image: 2htdp/image)
         lux
         lux/chaos/gui
         lux/chaos/gui/val
         lux/chaos/gui/key)

(define MODES
  '(pict image))

(struct demo
  (g/v mode)
  #:methods gen:word
  [(define (word-label s ft)
     (lux-standard-label "Values" ft))
   (define (word-output w)
     (match-define (demo g/v mode-n) w)
     (g/v
      (match (list-ref MODES mode-n)
        ['pict
         (pict:arrowhead 30 0)]
        ['image
         (image:add-line
          (image:rectangle 100 100 "solid" "darkolivegreen")
          25 25 75 75
          (image:make-pen "goldenrod" 30 "solid" "round" "round"))])))
   (define (word-event w e)
     (match-define (demo g/v mode-n) w)
     (define closed? #f)
     (cond
      [(eq? e 'close)
       #f]
      [(and (key-event? e)
            (not (eq? 'release (send e get-key-code))))
       (demo g/v (fxmodulo (fx+ 1 mode-n) (length MODES)))]
      [else
       (demo g/v mode-n)]))
   (define (word-tick w)
     w)])

(module+ main
  (call-with-chaos
    (make-gui 60.0)
   (λ () (fiat-lux (demo (make-gui/val) 0)))))
