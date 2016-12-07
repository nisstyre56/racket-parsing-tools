#! /usr/bin/racket
#lang racket

; prefix-tree based tokenizer

(struct PFTree
        (root children)
        #:transparent)

(define (str-head str)
  (if (= (string-length str) 0) (integer->char 0)
      (string-ref str 0)))

(define (str-tail str)
  (if (= (string-length str) 0) (integer->char 0)
  (substring str 1
             (string-length str))))

(define str-heads (curry map str-head))

(define str-tails (curry map str-tail))

(define (prefixes strs)
  (map cons
       (str-heads strs)
       (str-tails strs)))

; Create a prefix-tree from a list of strings
(define (prefix-tree strs)
  (match strs
    [(list x) x]
    [_
      (define ps (prefixes strs))

      (define sorted-ps
        (sort ps #:key car char<?))
      
      (filter
       (compose1 not false?)
       (map
        (lambda (group)

          (define sub-tree
            (PFTree
             (caar group)
             (prefix-tree
              (map cdr group))))

          (match sub-tree
            [(PFTree #\nul #\nul) #f]
            [_ sub-tree]))
        
        (group-by car sorted-ps)))]))

; Compress linear runs of nodes in a prefix-tree
; such that they are single nodes with the entire suffix left
(define (compress pftree)
  (match pftree
    [(PFTree (? char? left) (? string? right))
      (format "~a~a" left right)]
    [(? string?) pftree]
    [(PFTree root (list (PFTree subroot rest)))
      (PFTree (format "~a~a" root subroot) (compress rest))]
    [(PFTree root rest) (PFTree root (compress rest))]
    [(list (PFTree _ _) ...) (map compress pftree)]))

 (compress
  (prefix-tree
  '["abc" "art" "dart" "artsy" "damn"]))