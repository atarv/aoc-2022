#lang racket

;; Day 4: Camp Cleanup -- Part 2

(define (to-numbers lst) (map string->number lst))

(define (between? min max x) (and (<= min x) (>= max x)))

(define (overlaps? left right)
  (match-let ([(list left-min left-max) (to-numbers (string-split left "-"))]
              [(list right-min right-max) (to-numbers (string-split right "-"))])
    (or
     (between? left-min left-max right-min)
     (between? left-min left-max right-max)
     (between? right-min right-max left-min)
     (between? right-min right-max left-max))))

(let* ([input (port->lines)]
       [range-pairs (map (λ (line) (string-split line ",")) input)])
  (count (λ (pair) (apply overlaps? pair)) range-pairs))