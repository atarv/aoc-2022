#lang racket

;; Day 6: Tuning Trouble - Part 2

;; There's probably a cleaner way but whatever works right?
(define (in-packets-of-14 from-seq)
  (for/list ([a from-seq]
             [b (sequence-tail from-seq 1)]
             [c (sequence-tail from-seq 2)]
             [d (sequence-tail from-seq 3)]
             [e (sequence-tail from-seq 4)]
             [f (sequence-tail from-seq 5)]
             [g (sequence-tail from-seq 6)]
             [h (sequence-tail from-seq 7)]
             [i (sequence-tail from-seq 8)]
             [j (sequence-tail from-seq 9)]
             [k (sequence-tail from-seq 10)]
             [l (sequence-tail from-seq 11)]
             [m (sequence-tail from-seq 12)]
             [n (sequence-tail from-seq 13)])
    (list a b c d e f g h i j k l m n)))

(let* ([input (port->string)]
       [packets (in-packets-of-14 (in-string input))])
  ;; packet length has to be added since index finds the start of the marker
  (+ 14 (index-where packets (λ (packet) (= (set-count (list->set packet)) 14)))))