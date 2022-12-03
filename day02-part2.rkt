#lang racket

;; Day 2: Rock Paper Scissors - Part 2

(define (shape-score shape)
  (match shape
    ["A" 1] ;; Rock
    ["B" 2] ;; Paper
    ["C" 3])) ;; Scissors

(define (outcome-score outcome)
  (match outcome
    ["X" 0] ;; loss
    ["Y" 3] ;; draw
    ["Z" 6])) ;; win

(define (lose shape)
  (match shape
    ["A" "C"]
    ["B" "A"]
    ["C" "B"]))

(define (win shape)
  (match shape
    ["A" "B"]
    ["B" "C"]
    ["C" "A"]))

(define (determine-shape round)
  (let* ([opponent-shape (first round)]
         [preferred-outcome (second round)]
         [my-shape (match preferred-outcome
                     ["X" (lose opponent-shape)]
                     ["Y" opponent-shape]
                     ["Z" (win opponent-shape)])])
    my-shape))

(define (determine-score round)
  (+
   (outcome-score (second round))
   (shape-score (determine-shape round))))

(let* ([input (port->lines)]
       [rounds (map (λ (line) (string-split line " " )) input)]
       [scores (map determine-score rounds)])
  (apply + scores))
