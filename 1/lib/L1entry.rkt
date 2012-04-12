#!/usr/bin/env racket
#lang racket
(require racket/cmdline
         "L1parse.rkt"
         "L1compile.rkt"
         "L1types.rkt")

(define (main)
  (let
      ([filename (command-line #:args (filename) filename)])
    (compile (parse filename))))

;(main)
