#lang racket

;; Day 6: Tuning Trouble - Part 1

(define (in-packets-of-4 from-seq)
  (for/list ([a from-seq]
             [b (sequence-tail from-seq 1)]
             [c (sequence-tail from-seq 2)]
             [d (sequence-tail from-seq 3)])
    (list a b c d)))

(let* ([input (port->string)]
       [packets (in-packets-of-4 (in-string input))])
  ;; packet length has to be added since index finds the start of the marker
  (+ 4 (index-where packets (λ (packet) (= (set-count (list->set packet)) 4)))))