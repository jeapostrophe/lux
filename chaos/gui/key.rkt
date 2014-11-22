#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/class
         racket/gui/base
         racket/contract/base)

(struct key-state
  (keys
   shift? control? meta? alt?
   mod3? mod4? mod5?)
  #:mutable)

(define-syntax (set-key-state stx)
  (syntax-case stx ()
    [(_ ks ke id)
     (with-syntax ([set-key-state-id?!
                    (format-id #'id "set-key-state-~a?!" #'id)]
                   [get-id-down
                    (format-id #'id "get-~a-down" #'id)])
       (syntax/loc stx
         (set-key-state-id?! ks (send ke get-id-down))))]))
(define-syntax-rule (set-key-states ks ke (id ...))
  (begin (set-key-state ks ke id) ...))

(define (key-event? x)
  (is-a? x key-event%))

(define (key-state-update! ks ke)
  (define ht (key-state-keys ks))
  (match (send ke get-key-code)
    ['release
     (hash-set! ht (send ke get-key-release-code) #f)]
    [kc
     (hash-set! ht kc #t)])
  (set-key-states
   ks ke
   (shift control meta alt mod3 mod4 mod5)))

(define (make-key-state)
  (key-state (make-hasheq) #f #f #f #f #f #f #f))

(define (key-state-set? ks kc)
  (hash-ref (key-state-keys ks) kc #f))
(define (key-state-set?! ks kc)
  (begin0 (key-state-set? ks kc)
    (hash-set! (key-state-keys ks) kc #f)))

(provide
 (contract-out
  [struct key-state
    ([keys hash?]
     [shift? boolean?]
     [control? boolean?]
     [meta? boolean?]
     [alt? boolean?]
     [mod3? boolean?]
     [mod4? boolean?]
     [mod5? boolean?])]
  [key-event?
   (-> any/c boolean?)]
  [make-key-state
   (-> key-state?)]
  [key-state-update!
   (-> key-state? key-event?
       any)]
  [key-state-set?
   (-> key-state? (or/c char? key-code-symbol?)
       boolean?)]
  [key-state-set?!
   (-> key-state? (or/c char? key-code-symbol?)
       boolean?)]))
