#lang racket/gui
(require racket/contract/base)

(define nnint? nonnegative-integer?)
(provide
 (contract-out
  [get-mouse-x/y
   (->* ()
        ()
        (values nnint? nnint?))]
  [get-display-info
   (->* (nnint?)
        ()
        (values nnint? nnint? nnint? nnint?))]
  [find-monitor
   (->* (nnint? nnint?)
        ()
        (or/c false/c nnint?))]
  [find-mouse-monitor
   (->* ()
        ()
        (or/c false/c nnint?))]
  [find-x/y
   (->* ((or/c nnint? (one-of/c 'left 'center 'right))
         (or/c nnint? (one-of/c 'top  'center 'bottom)))
        (#:width
         nnint?
         #:height
         nnint?
         #:monitor
         (or/c false/c nnint?))
        (values nnint? nnint?))]))


;; Returns the coordinates of the mouse pointer
(define (get-mouse-x/y)
  (define-values (pt st)
    (get-current-mouse-state))
  (values (send pt get-x)
          (send pt get-y)))

;; Returns the position x, y and the sizes w, h of the provided display/monitor number
(define (get-display-info disp)
  (define-values (-x0 -y0)
         (if (= disp 0)
             (values 0 0) ; to avoid the bars and menus on first monitor
             (get-display-left-top-inset #:monitor disp)))
  (define-values (w h)
         (get-display-size #:monitor disp))
  (values (- -x0) (- -y0) w h))

;; Returns the display/monitor that contains the coordinates x,y, or #f if not found
(define (find-monitor x y)
  (define n-disp (get-display-count))
  (let loop ([disp 0])
    (cond
      [(>= disp n-disp) #f]
      [else
       (define-values (x0 y0 w h)
         (get-display-info disp))
       (if (and (<= x0 x (+ x0 w))
                (<= y0 y (+ y0 h)))
           disp
           (loop (+ disp 1)))])))

;; Returns the display/monitor number that contains the mouse pointer
(define (find-mouse-monitor)
  (define-values (pt st)
    (get-current-mouse-state))
  (find-monitor (send pt get-x) (send pt get-y)))

;; Returns the position x, y in pixels for where to place a frame of size fr-w, fr-h
;; on the specified monitor. If monitor is #f, then the monitor where the mouse
;; pointer is is used.
;; pos-x: (or/c non-negative-integer? (one-of 'left 'center 'right))
;; pos-y: (or/c non-negative-integer?
(define (find-x/y pos-x pos-y
                  #:width  [fr-w 0]
                  #:height [fr-h 0]
                  #:monitor [monitor #f])
  (when (and monitor (not (<= 0 monitor (- (get-display-count) 1))))
    (error "Invalid monitor number" monitor))
  (define disp (or monitor (find-mouse-monitor) 0))
  (define-values (disp-x0 disp-y0 disp-w disp-h)
    (get-display-info disp))
  (define x
    (cond [(eq? pos-x 'left)
           disp-x0]
          [(eq? pos-x 'center)
           (+ disp-x0 (quotient (- disp-w fr-w) 2))]
          [(eq? pos-x 'right)
           (+ disp-x0 disp-w (- fr-w))]
          [else (+ disp-x0 pos-x)]))
  (define y
    (cond [(eq? pos-y 'top)
           disp-y0]
          [(eq? pos-y 'center)
           (+ disp-y0 (quotient (- disp-h fr-h) 2))]
          [(eq? pos-y 'bottom)
           (+ disp-y0 disp-h (- fr-h))]
          [else (+ disp-y0 pos-y)]))
  (values x y))

(module+ drracket

  (define n-displays (get-display-count))
  #;(for/list ([d (in-range n-displays)])
    (define-values (x y)
      (get-display-left-top-inset #:monitor d))
    (define-values (w h)
      (get-display-size #:monitor d))
    (vars->assoc d x y w h))
  
  (define fr
    (new frame%
         [label "test"]
         [width 400]
         [height 100]))
  (send fr show true)
  (for* ([monitor (in-sequences (in-value #f)
                                (in-range (get-display-count)))]
         [xx (in-list '(0 100 300 left center right))]
         [yy (in-list '(0 200 400 top center bottom))])
    (displayln (list monitor xx yy))
    (define-values (x y)
      (find-x/y xx yy
                #:width (send fr get-width)
                #:height (send fr get-height)
                #:monitor monitor))
    (send fr set-label (format "~a, ~a(~a), ~a(~a)" monitor xx x yy y))
    (send fr move x y)
    (sleep/yield 1 #;0.5))
  )

