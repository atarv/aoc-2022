#lang racket

;; Day 2: Rock Paper Scissors - Part 1

(define-values (loss draw win) (values 0 3 6))
(define (determine-score shapes)
  (let ([shape-score (match (second shapes)
                       ["X" 1]
                       ["Y" 2]
                       ["Z" 3])]
        [outcome (match shapes
                   [(list "A" "X") draw]
                   [(list "A" "Y") win]
                   [(list "A" "Z") loss]
                   [(list "B" "X") loss]
                   [(list "B" "Y") draw]
                   [(list "B" "Z") win]
                   [(list "C" "X") win]
                   [(list "C" "Y") loss]
                   [(list "C" "Z") draw])])
    (+ outcome shape-score)))

(let* ([input (port->lines)]
       [rounds (map (λ (line) (string-split line " " )) input)]
       [scores (map determine-score rounds)])
  (apply + scores))
