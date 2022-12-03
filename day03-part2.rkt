#lang racket

;; Day 3: Rucksack Reorganization - Part 2

(define (find-common-item . compartments)
  (set-first (apply set-intersect compartments)))

(define (chunks-of lst n)
  (if (not (empty? lst))
      (cons (take lst n) (chunks-of (drop lst n) n))
      '()))

(define (char-between? min max x) (and (char<=? min x) (char>=? max x)))

(define (priority item)
  (cond
    [(char-between? #\a #\z item) (- (char->integer item) (char->integer #\a) -1)]
    [(char-between? #\A #\Z item) (- (char->integer item) (char->integer #\A) -27)]))

(let* ([lines (port->lines)]
       [rucksacks (map string->list lines)]
       [elf-groups (chunks-of rucksacks 3)]
       [badges (map (λ (rucksacks) (apply find-common-item rucksacks)) elf-groups)]
       [priorities (map priority badges)])
  (apply + priorities))