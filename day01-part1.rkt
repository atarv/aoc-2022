#lang racket
;; Day 1: Calorie Counting

(let* ([input (port->string)]
       [calories (map
                  (λ (str) (map string->number (string-split str "\n")))
                  (string-split input "\n\n"))]
       [calory-sums (map (λ (group) (apply + group)) calories)])
  (apply max calory-sums))
