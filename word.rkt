#lang racket/base
(require racket/list
         racket/match
         racket/contract/base
         racket/flonum
         racket/format
         racket/generic
         lux/chaos)

(define-generics word
  (word-fps word)
  (word-label word frame-time)
  (word-evt word)
  (word-event word evt)
  (word-tick word)
  (word-output word)
  (word-return word)
  #:fallbacks
  [(define (word-fps w)
     60.0)
   (define (word-label w frame-time)
     (lux-standard-label "Lux" frame-time))
   (define (word-evt w)
     never-evt)
   (define (word-event w e) w)
   (define (word-tick w) w)
   (define (word-output w) #f)
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
  (chaos-start! c)
  (parameterize ([current-chaos c])
    (begin0 (t)
      (chaos-stop! c))))

(define (fiat-lux w)
  (define c (current-chaos))
  (unless c
    (error 'fiat-lux "Not called within call-with-chaos"))
  (factum-fiat-lux c w))

(define (factum-fiat-lux c w)
  (define (update-word w f make-next-time)
    (define start-time (current-inexact-milliseconds))
    (define new-w (f w))
    (match new-w
      [#f
       (word-return w)]
      [_
       (chaos-output! c (word-output new-w))
       (define end-time (current-inexact-milliseconds))
       (define frame-time (fl- end-time start-time))
       (define new-label
         (word-label new-w frame-time))
       (chaos-label! c new-label)
       (define next-time (make-next-time new-w start-time))
       (body next-time new-w)]))
  (define (compute-next-time start-time fps)
    (define time-incr (fl* (fl/ 1.0 fps) 1000.0))
    (define next-time (fl+ start-time time-incr))
    next-time)
  (define (body next-time w)
    (define input-evt
      (handle-evt
       (choice-evt (word-evt w)
                   (chaos-event c))
       (λ (e)
         (update-word w
                      (λ (w)
                        (word-event w e))
                      (λ (new-w start-time)
                        (define old-fps (word-fps w))
                        (define fps (word-fps new-w))
                        (if (= old-fps fps)
                            next-time
                            (compute-next-time start-time fps)))))))
    (define refresh-evt
      (handle-evt
       (alarm-evt next-time)
       (λ (_)
         (update-word w
                      word-tick
                      (λ (new-w start-time)
                        (define fps (word-fps new-w))
                        (compute-next-time start-time fps))))))
    (sync/timeout
     (λ ()
       (chaos-yield
        c
        (choice-evt input-evt refresh-evt)))
     input-evt))
  (chaos-swap! c (λ () (body 0 w))))

(provide
 gen:word
 (contract-out
  [word?
   (-> any/c word?)]
  [lux-standard-label
   (-> string? flonum?
       string?)]
  [call-with-chaos
   (-> chaos? (-> any)
       any)]
  [fiat-lux
   (-> word?
       any)]))
