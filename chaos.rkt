#lang racket/base
(require racket/contract/base
         racket/sequence
         racket/generic)

(define-generics chaos
  (chaos-fps chaos)
  (chaos-yield chaos evt)
  (chaos-inputs chaos)
  (chaos-output! chaos outputs)
  (chaos-label! chaos label)
  (chaos-swap! chaos thunk)
  #:fallbacks
  [(define (chaos-fps c)
     60.0)
   (define (chaos-yield c e)
     (sync e))
   (define (chaos-inputs c)
     empty-sequence)
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
  [chaos-fps (-> chaos? flonum?)]
  [chaos-yield (-> chaos? evt? any)]
  [chaos-inputs (-> chaos? sequence?)]
  [chaos-output! (-> chaos? any/c any)]
  [chaos-label! (-> chaos? string? any)]
  [chaos-swap! (-> chaos? (-> any) any)]))
