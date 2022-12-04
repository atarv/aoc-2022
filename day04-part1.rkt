#lang racket

;; Day 4: Camp Cleanup -- Part 1

(define (to-numbers lst) (map string->number lst))

(define (fully-contains? left right)
  (match-let ([(list left-min left-max) (to-numbers (string-split left "-"))]
              [(list right-min right-max) (to-numbers (string-split right "-"))])
    (or
     (and (<= left-min right-min) (>= left-max right-max))
     (and (<= right-min left-min) (>= right-max left-max)))))

(let* ([input (port->lines)]
       [range-pairs (map (λ (line) (string-split line ",")) input)])
  (count (λ (pair) (apply fully-contains? pair)) range-pairs))