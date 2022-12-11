#lang racket

;; Day 9: Rope Bridge - Part 1

(struct motion (direction amount) #:transparent)

(define tail-position (cons 0 0))
(define head-position (cons 0 0))
(define visited-tail-positions (mutable-set tail-position))

(define (parse-motion line)
  (match-let ([(list dir amt-str) (string-split line " ")])
    (motion dir (string->number amt-str))))

(define (bimap pair fst snd)
  (match-let ([(cons a b) pair]) (cons (fst a) (snd b))))

(define (map-first pair fn)
  (bimap pair fn identity))

(define (map-second pair fn)
  (bimap pair identity fn))

(define (move-head direction)
  (define (update-head select fn) (set! head-position (select head-position fn)))
  (match direction
    ["U" (update-head map-first add1)]
    ["D" (update-head map-first sub1)]
    ["L" (update-head map-second sub1)]
    ["R" (update-head map-second add1)]))

(define (n-times n action . args)
  (for ([_ (in-range 0 n)]) (apply action args)))

(define (follow-head)
  (define (update-tail fst snd) (set! tail-position (bimap tail-position fst snd)))
  (define (select-update-fn n) (if (zero? n) identity (if (positive? n) add1 sub1)))
  (match-let ([(cons head-row head-col) head-position]
              [(cons tail-row tail-col) tail-position])
    (let ([row-diff (- head-row tail-row)] [col-diff (- head-col tail-col)])
      (unless (and (<= (abs row-diff) 1) (<= (abs col-diff) 1))
        (update-tail (select-update-fn row-diff) (select-update-fn col-diff))
        (set-add! visited-tail-positions tail-position)))))

(define (interpret-motion motion)
  (n-times (motion-amount motion)
           (λ () (begin
                   (move-head (motion-direction motion))
                   (follow-head)))))

(let* ([input (port->lines)]
       [motions (map parse-motion input)])
  (map interpret-motion motions)
  (set-count visited-tail-positions))