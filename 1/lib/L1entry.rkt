#lang racket
(require racket/cmdline
         "L1compile.rkt"
         "L1parse.rkt"
         "L1types.rkt")

(define (main)
  (let
      ([filename (command-line #:args (filename) filename)])
    (display (compile (parse filename)))))

(main)
