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

;; XXX In the process of adding (collect-garbage? #t) to this, I
;; noticed a problem with the way that things are timed. Right now, if
;; an input event occurs, then the alarm can be put off. Also, the
;; output occurs on input events even when there is an FPS. What
;; really should happen, however, is that output should only occur at
;; the FPS and the alarm deadlines should never reset. It needs to be
;; considerably changed to fix that. I did something in the last
;; reverted commit, but I don't likw it.
(define (compute-next-time start-time fps)
  (define time-incr (fl* (fl/ 1.0 fps) 1000.0))
  (define next-time (fl+ start-time time-incr))
  next-time)

(define (factum-fiat-lux c w)
  (define (continue-or-word-return next-w old-w k)
    (cond
      [(not next-w)
       ((LOG! word-return) old-w)]
      [else
       (k next-w)]))
  (define (output&process-input&wait frame-start-time w)
    (chaos-output! c ((LOG! word-output) w))
    (define frame-end-time (current-inexact-milliseconds))
    (define frame-time (- frame-end-time frame-start-time))
    (define new-label ((LOG! word-label) w frame-time))
    (chaos-label! c new-label)

    (define fps ((LOG! word-fps) w))
    (define next-time (compute-next-time frame-end-time fps))
    (define deadline-evt (alarm-evt next-time))
    (define input-enabled? (zero? fps))

    (define w-evt ((LOG! word-evt) w))
    (define c-evt (chaos-event c))
    (define w-or-c-evt (choice-evt w-evt c-evt))

    (let process-input&wait ([w w])
      (define wait-evt
        (handle-evt deadline-evt
                    (λ (_)
                      (define next-w ((LOG! word-tick) w))
                      (continue-or-word-return
                       next-w w
                       (λ (next-w)
                         (output&process-input&wait frame-end-time next-w))))))
      (define input-evt
        (handle-evt w-or-c-evt
                    (λ (e)
                      (define next-w ((LOG! word-event) w e))
                      (continue-or-word-return
                       next-w w
                       (λ (next-w)
                         (if input-enabled?
                             (output&process-input&wait frame-end-time next-w)
                             (process-input&wait next-w)))))))
      (define both-evt
        (choice-evt input-evt wait-evt))
      (sync/timeout
       (λ () (chaos-yield c both-evt))
       input-evt)))

  (chaos-swap! c (λ () (output&process-input&wait (current-inexact-milliseconds) w))))

(define-syntax-rule (LOG! id)
  (begin (LOG!* 'id) id))
(define (LOG!* i)
  (writeln (cons (current-inexact-milliseconds) i))
  (flush-output))

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
