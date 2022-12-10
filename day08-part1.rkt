#lang racket

;; Day 8: Treetop Tree House - Part 1

(struct tree (height [visible #:mutable #:auto] ) #:transparent)

(define (parse-trees lines)
  (for/vector #:length (length lines) ([line (in-list lines)])
    (for/vector #:length (string-length line) ([char (in-string line)])
      (tree (string->number (string char))))))

(define (trees-size trees)
  (cons (vector-length (vector-ref trees 0)) (vector-length trees)))

(define (line-visibility! treeline)
  (let ([current-max-height -1]
        [in-list-or-vector (if (vector? treeline) in-vector in-list)])
    (for ([t (in-list-or-vector treeline)])
      (if ((tree-height t) . > . current-max-height)
          (begin
            (set-tree-visible! t #t)
            (set! current-max-height (tree-height t)))
          (void)))))

(define (check-visibility! trees from-direction)
  (match-let ([(cons width _height) (trees-size trees)])
    (match from-direction
      ['up (vector-map
            line-visibility!
            (for/vector ([i (in-range 0 width)])
              (vector-map (λ (line) (vector-ref line i)) trees)))]
      ['right (vector-map (λ (line) (line-visibility! (reverse (vector->list line)))) trees)]
      ['bottom'
              (void) ;; This line is somehow important to get correct results...
              (for/vector ([i (in-range 0 width)])
                (line-visibility!  (reverse (vector->list (vector-map (λ (line) (vector-ref line i)) trees)))))]
      ['left (vector-map line-visibility! trees)])))

(define (count-visible trees)
  (apply + (vector->list (vector-map (λ (line) (vector-count tree-visible line)) trees))))

(let* ([input (port->lines)]
       [trees (parse-trees input)])
  (for/list ([direction (list 'up 'right 'bottom 'left)])
    (check-visibility! trees direction))
  (displayln (count-visible trees)))