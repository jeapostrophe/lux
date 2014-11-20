#lang racket/base
(require racket/list
         racket/match
         racket/contract/base
         racket/flonum
         racket/format
         racket/generic
         lux/chaos)

(define-generics word
  (word-label word frame-time)
  (word-event word evt)
  (word-tick word)
  (word-output word)
  (word-return word)
  #:fallbacks
  [(define (word-label w frame-time)
     (lux-standard-label "Lux" frame-time))
   (define (word-event w e) w)
   (define (word-tick w) w)
   (define (word-output w) empty)
   (define (word-return w) w)])

(define (lux-standard-label l frame-time)
  (~a l
      ": "
      "Frame time: "
      (~r frame-time
          #:min-width 5
          #:precision 1)
      "ms; "
      "FPS: "
      (~r (fl/ 1000.0 frame-time)
          #:min-width 10
          #:precision 2)))

(define current-chaos (make-parameter #f))

(define (call-with-chaos c t)
  (parameterize ([current-chaos c])
    (t)))

(define (fiat-lux w)
  (define c (current-chaos))
  (unless c
    (error 'fiat-lux "Not called within call-with-chaos"))
  (factum-fiat-lux c w))

(define (factum-fiat-lux c w)
  (define fps (chaos-fps c))
  (define time-incr (fl* (fl/ 1.0 fps) 1000.0))
  (define (update-word w make-next-tick-evt f)
    (define start-time (current-inexact-milliseconds))
    (define new-w (f w))
    (match new-w
      [#f
       (word-return w)]
      [_
       (chaos-output! c (word-output w))
       (define end-time (current-inexact-milliseconds))
       (define frame-time (fl- end-time start-time))
       (define new-label
         (word-label new-w frame-time))
       (chaos-label! c new-label)
       (define next-tick-evt (make-next-tick-evt start-time))
       (body next-tick-evt new-w)]))
  (define (body tick-evt w)
    (chaos-yield
     c
     (choice-evt
      (handle-evt
       (chaos-event c)
       (λ (e)
         (update-word w
                      (λ (start-time)
                        tick-evt)
                      (λ (w)
                        (word-event w e)))))
      (handle-evt
       tick-evt
       (λ (_)
         (update-word w
                      (λ (start-time)
                        (define next-time (fl+ start-time time-incr))
                        (define next-tick-evt (alarm-evt next-time))
                        next-tick-evt)
                      word-tick))))))
  (chaos-swap! c (λ () (body always-evt w))))

(provide
 gen:word
 (contract-out
  [lux-standard-label
   (-> string? flonum?
       string?)]
  [call-with-chaos
   (-> chaos? (-> any)
       any)]
  [fiat-lux
   (-> word?
       any)]))
