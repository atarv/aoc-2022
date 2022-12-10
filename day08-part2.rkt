#lang racket

;; Day 8: Treetop Tree House - Part 2

(struct tree (height [score #:mutable #:auto] ) #:transparent)

(define (parse-trees lines)
  (for/vector #:length (length lines) ([line (in-list lines)])
    (for/vector #:length (string-length line) ([char (in-string line)])
      (tree (string->number (string char))))))

(define (trees-size trees)
  (cons (vector-length (vector-ref trees 0)) (vector-length trees)))

(define (view-from-location-to trees row col direction)
  (match-let ([(cons width height) (trees-size trees)])
    (drop (match direction
            ['up
             (for/list ([r (in-inclusive-range row 0 -1)]) (vector-ref (vector-ref trees r) col))]
            ['down
             (for/list ([r (in-range row height)]) (vector-ref (vector-ref trees r) col))]
            ['right
             (for/list ([c (in-range col width)]) (vector-ref (vector-ref trees row) c))]
            ['left
             (for/list ([c (in-inclusive-range col 0 -1)]) (vector-ref (vector-ref trees row) c))])
          1))) ;; drop tree in current location

(define (scenic-score-view view current-height)
  (let*-values ([(included excluded)
                 (splitf-at view (λ (t) (current-height . > . (tree-height t))))]
                [(score)
                 (+
                  (length included)
                  (if (and ((length excluded) . > . 0)
                           ((tree-height (first excluded)) . >= . current-height))
                      1
                      0))])
    score))


(define (scenic-score! trees row col)
  (let* ([tree (vector-ref (vector-ref trees row) col)]
         [current-tree-height (tree-height tree)]
         [score (apply *
                       (map
                        (λ (direction) (scenic-score-view
                                        (view-from-location-to trees row col direction)
                                        current-tree-height))
                        (list 'up 'right 'down 'left)))])
    (set-tree-score! tree score)))

(define (max-score trees)
  (define (vector-max vector) (apply max 0 (vector->list vector)))
  (define (max-score-line line)
    (vector-max (vector-filter identity (vector-map tree-score line))))
  (vector-max (vector-map max-score-line trees)))

(let* ([input (port->lines)]
       [trees (parse-trees input)])
  (match-let ([(cons width height) (trees-size trees)])
    (for* ([i (in-range 1 (sub1 height))] [j (in-range 1 (sub1 width))])
      (scenic-score! trees i j)))
  (max-score trees))