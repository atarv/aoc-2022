#lang racket

;; Day 3: Rucksack Reorganization - Part 1

(define (parse-compartments str)
  (let* ([compartment-size (/ (string-length str) 2)]
         [chars (string->list str)]
         [first-compartment (take chars compartment-size)]
         [second-compartment (drop chars compartment-size)])
    (list first-compartment second-compartment)))

(define (find-common-item first-compartment second-compartment)
  (set-first (set-intersect first-compartment second-compartment)))

(define (char-between? min max x) (and (char<=? min x) (char>=? max x)))

(define (priority item)
  (cond
    [(char-between? #\a #\z item) (- (char->integer item) (char->integer #\a) -1)]
    [(char-between? #\A #\Z item) (- (char->integer item) (char->integer #\A) -27)]))

(let* ([lines (port->lines)]
       [compartments (map parse-compartments lines)]
       [common-items (map (λ (comparment-pair) (apply find-common-item comparment-pair)) compartments)]
       [priorities (map priority common-items)])
  (apply + priorities))