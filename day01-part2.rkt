#lang racket
;; Day 1: Calorie Counting

(let* ([input (port->string (current-input-port))]
       [calories (map
                  (lambda (str) (map string->number (port->lines (open-input-string str))))
                  (string-split input #px"\n\n"))]
       [calory-sums (map (lambda (group) (apply + group)) calories)])
  (apply + (take (sort calory-sums >) 3)))
