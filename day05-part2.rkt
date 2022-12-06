#lang racket

;; Day 5: Supply Stacks - Part 2

(require data/gvector)

(struct instruction (amount from to) #:transparent)

(define (parse-stacks str)
  (let* ([lines (string-split str "\n")]
         [column-count (length (string-split (last lines) #rx" +"))]
         [stacks (make-hash (build-list column-count (λ (i) (cons (add1 i) (make-gvector)))))]
         [stack-levels (drop-right lines 1)]) ;; remove index line
    (for ([level (in-list stack-levels)])
      (for ([crate-slot (in-range 1 (* column-count 4) 4)]
            [col (in-inclusive-range 1 column-count)]
            #:when (not (equal? (string-ref level crate-slot) #\space)))
        (let ([crate (string-ref level crate-slot)])
          (hash-update! stacks col (λ (stack) (gvector-add! stack crate) stack)))))
    stacks))

(define (parse-instructions str)
  (let ([lines (string-split str "\n")])
    (for/list ([line lines]
               #:when (not (equal? line "")))
      (let ([param-strs (first (regexp-match* #px"move (\\d+) from (\\d+) to (\\d+)"
                                              line
                                              #:match-select rest))])
        (apply instruction (map string->number param-strs))))))

(define (peek stack)
  (gvector-ref stack 0))

(define (pop! stack)
  (let ([value (peek stack)])
    (gvector-remove! stack 0)
    value))

(define (push! stack value)
  (gvector-insert! stack 0 value))

(define (n-times n action . args)
  (for ([_ (in-range n)])
    (apply action args)))

(define (interpret instructions stacks)
  (for ([instr (in-list instructions)])
    (let ([from-stack (hash-ref stacks (instruction-from instr))]
          [to-stack (hash-ref stacks (instruction-to instr))]
          [crates (make-gvector)])
      (n-times (instruction-amount instr) (λ () (push! crates (pop! from-stack))))
      (for ([crate (in-list (gvector->list crates))]) (push! to-stack crate)))))

(let*-values ([(input) (port->string)]
              [(stacks-str instructions-str) (apply values (string-split input #rx"\n\n"))]
              [(stacks) (parse-stacks stacks-str)]
              [(instructions) (parse-instructions instructions-str)])
  (interpret instructions stacks)
  (let ([stack-tops (sequence->list (sequence-map peek (in-hash-values stacks)))])
    (displayln (list->string stack-tops))))
