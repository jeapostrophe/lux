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

(define (compute-next-time start-time fps)
  (define time-incr (fl* (fl/ 1.0 fps) 1000.0))
  (define next-time (fl+ start-time time-incr))
  next-time)

(define (continue-or-word-return next-w old-w k)
  (cond
    [(not next-w)
     (word-return old-w)]
    [else
     (k next-w)]))

(define (factum-fiat-lux c w)
  (define (output&process-input&wait frame-start-time w)
    (define pre-output-time (current-inexact-milliseconds))
    (chaos-output! c (word-output w))
    (define frame-end-time (current-inexact-milliseconds))
    (define frame-time (- frame-end-time frame-start-time))
    #;(printf "W: ~v\tG: ~v\tT: ~v\n"
            (- pre-output-time frame-start-time)
            (- frame-end-time pre-output-time)
            frame-time)
    (define new-label (word-label w frame-time))
    (chaos-label! c new-label)

    ;; Ideally we could compute how much time we have available for GC
    ;; and just use that so we never have any pauses. That's a very
    ;; big wish though.
    (collect-garbage 'incremental)

    (define fps (word-fps w))
    (define next-time (compute-next-time frame-end-time fps))
    (define deadline-evt (alarm-evt next-time))
    (define input-enabled? (fl= 0.0 fps))

    (define w-evt (word-evt w))
    (define c-evt (chaos-event c))
    (define w-or-c-evt (choice-evt w-evt c-evt))

    (define continue
      (λ (next-w)
        (output&process-input&wait frame-end-time next-w)))

    (define THE-W w)
    (define wait-evt
      (handle-evt deadline-evt
                  (λ (_)
                    (define next-w (word-tick THE-W))
                    (continue-or-word-return
                     next-w THE-W
                     continue))))
    (define input-continue
      (λ (next-w)
        (cond
          [input-enabled?
           (output&process-input&wait frame-end-time next-w)]
          [else
           (set! THE-W next-w)
           (process-input&wait)])))
    (define input-evt
      (handle-evt w-or-c-evt
                  (λ (e)
                    (define next-w (word-event THE-W e))
                    (continue-or-word-return
                     next-w THE-W
                     input-continue))))
    (define both-evt
      (choice-evt input-evt wait-evt))
    (define timeout-f
      (λ () (chaos-yield c both-evt)))
    (define (process-input&wait)
      (sync/timeout timeout-f input-evt))

    (process-input&wait))

  (chaos-swap! c (λ () (output&process-input&wait (current-inexact-milliseconds) w))))

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
