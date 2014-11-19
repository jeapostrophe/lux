#lang racket/base
(require racket/list
         racket/match
         racket/contract/base
         racket/flonum
         racket/format
         racket/generic)

;; xxx abstract away sources (input events) and sinks (gui and sound)?
(define draw-mode/c
  (one-of/c 'draw 'gl))

(define-generics word
  (word-label word frame-time)
  (word-fps word)
  (word-tick word events)
  (word-draw-mode word)
  (word-draw! word width height dc)
  (word-pause word)
  (word-resume word state)
  (word-stop? word)
  (word-value word)
  #:fallbacks
  [(define (word-label w frame-time)
     (lux-standard-label "Lux" frame-time))
   (define (word-fps w)
     60.0)
   (define (word-tick w es) w)
   (define (word-draw-mode w) 'draw)
   (define (word-draw! w width height dc) (void))
   (define (word-pause w) w)
   (define (word-resume w) w)
   (define (word-stop? w) #f)
   (define (word-value w) w)])

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
          #:min-width 7
          #:precision 2)))

(define current-world (make-parameter #f))
(struct world (t ch))
(struct message (w pmz return-t return-ch time-evt))
(define-syntax-rule (call pmz e)
  (call-with-continuation-barrier
   (λ () (call-with-parameterization pmz (λ () e)))))
(define (start-world)
  (define submit-ch (make-channel))
  (define the-gui (start-gui))
  (define (body old-stack)
    ;; xxx i dislike that i don't know if old-stack is '()
    (gui-yield
     the-gui
     (choice-evt
      (match old-stack
        ['()
         never-evt]
        [(cons old-m stack)
         (match-define (message w pmz return-t return-ch time-evt) old-m)
         (handle-evt
          time-evt
          (λ (_)
            (define start-time (current-inexact-milliseconds))
            (define es (gui-events the-gui))
            ;; I fear that (call pmz e) is slow and I do it a lot
            ;; here. So maybe change tick to return all this stuff?
            (define new-w (call pmz (word-tick w es)))
            (gui-draw!
             the-gui
             (call pmz (word-draw-mode new-w))
             (λ (width height dc)
               (call pmz (word-draw! new-w width height dc))))
            (define end-time (current-inexact-milliseconds))
            (define frame-time (fl- end-time start-time))
            (define new-label
              (call pmz (word-label new-w frame-time)))
            (gui-label! the-gui new-label)
            (match (call pmz (word-stop? new-w))
              [#f
               (define fps (call pmz (word-fps new-w)))
               (define next-time (fl+ start-time (fl* (fl/ 1.0 fps) 1000.0)))
               (define next-time-evt (alarm-evt next-time))
               (define new-m
                 (message new-w pmz return-t return-ch next-time-evt))
               (body (cons new-m stack))]
              [#t
               (thread-resume return-t)
               (channel-put return-ch (word-value w))
               (body
                (match stack
                  ['()
                   stack]
                  [(cons old-m stack)
                   (match-define (message w pmz return-t return-ch time-evt) old-m)
                   (cons (message (call pmz (word-resume w))
                                  pmz return-t return-ch time-evt)
                         stack)]))])))])
      (handle-evt
       submit-ch
       (λ (new-m)
         (body
          (cons new-m
                (match old-stack
                  ['()
                   old-stack]
                  [(cons old-m stack)
                   (match-define (message w pmz return-t return-ch time-evt) old-m)
                   (cons (message (call pmz (word-pause w))
                                  pmz return-t return-ch time-evt)
                         stack)]))))))))
  (define world-t
    (thread
     (λ ()
       (body empty))))
  (world world-t submit-ch))

(define (fiat-lux w)
  (if (current-world)
      (fictio-fiat-lux w)
      (factum-fiat-lux w)))

(define (factum-fiat-lux w)
  (parameterize ([current-world (start-world)])
    (fictio-fiat-lux w)))
;; xxx this isn't going to work because the server won't be listening inside tick
(define (fictio-fiat-lux w)
  (define return-ch (make-channel))
  (match-define (world world-t submit-ch) (current-world))
  (thread-resume world-t)
  (define m (message w (current-parameterization)
                     (current-thread) return-ch
                     always-evt))
  (channel-put! submit-ch m)
  (channel-get return-ch))
