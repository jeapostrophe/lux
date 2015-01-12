#lang info
(define collection "lux")
(define deps '("draw-lib"
               "drracket"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               "base"
               "rackunit-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "htdp-doc"
                     "pict-doc"
                     "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/lux.scrbl" ())))
(define pkg-desc "a simple library for creating real-time graphical apps")
(define version "0.0")
(define pkg-authors '(jay))
