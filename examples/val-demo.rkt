#lang racket/base
(require racket/match
         racket/fixnum
         racket/gui/base
         racket/class
         (prefix-in pict: pict)
         (prefix-in image: 2htdp/image)
         lux
         lux/chaos/gui
         lux/chaos/gui/val)

(define MODES
  '(pict image))

(struct demo
  (mode)
  #:methods gen:word
  [(define (word-label s ft)
     (lux-standard-label "Values" ft))
   (define (word-output w)
     (match-define (demo mode-n) w)
     (match (list-ref MODES mode-n)
       ['pict
        (pict:arrowhead 30 0)]
       ['image
        (image:add-line
         (image:rectangle 100 100 "solid" "darkolivegreen")
         25 25 75 75
         (image:make-pen "goldenrod" 30 "solid" "round" "round"))]))
   (define (word-event w e)
     ;; xxx remove mutation
     (match-define (demo mode-n) w)
     (define closed? #f)
     (match e
       ['close
        (set! closed? #t)]
       [(? (λ (x) (is-a? x key-event%)) ke)
        (unless (eq? 'release (send ke get-key-code))
          (set! mode-n (fxmodulo (fx+ 1 mode-n) (length MODES))))]
       [_
        (void)])
     (match closed?
       [#t
        #f]
       [#f
        (demo mode-n)]))
   (define (word-tick w)
     w)])

(module+ main
  (call-with-chaos
   (make-gui/value (make-gui 60.0))
   (λ () (fiat-lux (demo 0)))))
