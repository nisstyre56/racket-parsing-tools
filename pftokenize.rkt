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
      (map
        (lambda (group)
          (displayln "group")
          (displayln group)
          (PFTree
            (caar group)
            (prefix-tree
              (map cdr group))))
        (group-by car sorted-ps))]))
