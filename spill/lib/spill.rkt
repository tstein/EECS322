#lang racket
(require "types.rkt")

(define spilled 0)
(define spillct 0)
(define sprefix "deadbeef")

(define (get-spill)
  (let ([sval spilled])
    (set! spilled (- spilled 4))
    sval))

(define/contract (make-tvar prefix)
  (-> symbol? string?)
  (let ([spillnum spillct])
    (set! spillct (+ spillct 1))
    (string-append
     (symbol->string prefix)
     (number->string spillnum))))

(define (get-spill-bits)
  (let ([sval (get-spill)])
    (list (make-tvar sprefix)
          sval)))

(define (spill code var offset prefix)
  (begin
    (set! spilled offset)
    (set! sprefix prefix)
    (flatten (map
              (λ (x) (spill-instr x var offset prefix))
              code))))

(define (spill-instr i v o p)
  (cond
    [(assign? i) (spill-assign i v)]
    [(mathop? i) (spill-mathop i v)]
    [(cmp? i) (spill-cmp i v)]
    [(goto? i) (spill-goto i v)]
    [(cjump? i) (spill-cjump i v)]
    [(call? i) (spill-call i v)]
    [(tail-call? i) (spill-tail-call i v)]
    [(print? i) (spill-print i v)]
    [(allocate? i) (spill-allocate i v)]
    [(array-error? i) (spill-array-error i v)]
    [else i]))

(define/contract (spill-assign i v)
  (-> l2instr? symbol? (listof l2instr?))
  (let ([dst (assign-dst i)]
        [src (assign-src i)])
    (cond
      [(eq? dst src) '()]
      [(eq? dst v) (let ([bits (get-spill-bits)])
                     (let ([tvar (string->symbol (car bits))]
                           [offset (cadr bits)])
                       (list (assign tvar src)
                             (assign (mem `ebp offset) tvar))))])))

(define (spill-mathop i v) `deadbeef)
(define (spill-cmp i v) `deadbeef)
(define (spill-goto i v) `deadbeef)
(define (spill-cjump i v) `deadbeef)
(define (spill-call i v) `deadbeef)
(define (spill-tail-call i v) `deadbeef)
(define (spill-print i v) `deadbeef)
(define (spill-allocate i v) `deadbeef)
(define (spill-array-error i v) `deadbeef)

(provide spill)
