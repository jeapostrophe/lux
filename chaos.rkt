#lang racket/base
(require racket/contract/base
         racket/generic)

(define-generics chaos
  (chaos-yield chaos evt)
  (chaos-event chaos)
  (chaos-output! chaos outputs)
  (chaos-label! chaos label)
  (chaos-swap! chaos thunk)
  #:fallbacks
  [(define (chaos-yield c e)
     (sync e))
   (define (chaos-event c)
     never-evt)
   (define (chaos-output! c os)
     (void))
   (define (chaos-label! c l)
     (void))
   (define (chaos-swap! chaos thunk)
     (thunk))])

(provide
 gen:chaos
 (contract-out
  [chaos? (-> any/c boolean?)]
  [chaos-yield (-> chaos? evt? any)]
  [chaos-event (-> chaos? evt?)]
  [chaos-output! (-> chaos? any/c any)]
  [chaos-label! (-> chaos? string? any)]
  [chaos-swap! (-> chaos? (-> any) any)]))
