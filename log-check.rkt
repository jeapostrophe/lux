#lang racket/base
(require racket/file
         racket/list
         racket/function
         racket/match
         math/base)

;; The idea here is that the average gap should be about 16.6. In the
;; first version, the numbers were:
;;
;; 1734 Events
;; word-evt: #294, average gap 34.903754432860495
;; word-tick: #27, average gap 43.47511643629808
;; word-output: #293, average gap 34.90750665534033
;; word-label: #293, average gap 33.786167092519264
;; word-fps: #559, average gap 17.674883442540324
;; word-event: #267, average gap 37.07471437382519
;; word-return: #1, average gap 0
;;
;; This is really bad. The goal should be for the counts for
;; word-(evt,output,label,fps) to all be the same as tick

(module+ main
  (require racket/runtime-path)
  (define-runtime-path log-p "log")
  
  (define events (file->list log-p))

  (printf "~a Events\n" (length events))

  (define label->times (make-hasheq))
  (for ([e (in-list events)])
    (match-define (cons t i) e)
    (hash-update! label->times i (curry cons t) empty))

  (define (average l)
    (if (empty? l)
        0
        (/ (sum l) (length l))))

  (define (gaps ts)
    (let loop ([last #f]
               [ts (sort ts <=)])
      (cond
        [(empty? ts)
         empty]
        [(not last)
         (loop (first ts) (rest ts))]
        [else
         (define t (first ts))
         (cons (- t last)
               (loop t (rest ts)))])))
  
  (define (average-gap ts)
    (average (gaps ts)))
  (for ([(i ts) (in-hash label->times)])
    (printf "~a: #~a, average gap ~a\n"
            i
            (length ts)
            (average-gap ts))))
