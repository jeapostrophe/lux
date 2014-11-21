#lang racket/base
(require racket/generic
         racket/match
         racket/sequence
         racket/contract/base
         lux/chaos)

(struct pair (l r)
        #:methods gen:chaos
        [(define/generic super-yield chaos-yield)
         (define/generic super-inputs chaos-inputs)
         (define/generic super-output! chaos-output!)
         (define/generic super-label! chaos-label!)
         (define/generic super-swap! chaos-swap!)
         (define (chaos-yield c e)
           (match-define (pair l r) c)
           (super-yield l
                        (handle-evt always-evt
                                    (λ (_)
                                      (super-yield r e)))))
         (define (chaos-inputs c)
           (match-define (pair l r) c)
           (sequence-append (super-inputs l)
                            (super-inputs r)))
         (define (chaos-output! c o)
           (match-define (pair l r) c)
           (match-define (cons l.o r.o) o)
           (super-output! l l.o)
           (super-output! r r.o))
         (define (chaos-label! c lab)
           (match-define (pair l r) c)
           (super-label! l lab)
           (super-label! r lab))
         (define (chaos-swap! c t)
           (match-define (pair l r) c)
           (super-swap! l (λ () (super-swap! r t))))])

(define (make-pair l r)
  (pair l r))
(provide
 (contract-out
  [make-pair
   (-> chaos? chaos?
       chaos?)]))
