#lang racket
(require racket/contract)

(define-struct/contract label ([name symbol?]))
(define-struct/contract mem ([addr symbol?]
                             [offset integer?]))

(define s? (or/c symbol?
                 integer?
                 label?))
(define t? (or/c symbol?
                 integer?))
(define u? (or/c symbol?
                 label?))

(define-struct/contract return ())
(define-struct/contract call ([func u?]))
(define-struct/contract tail-call ([func u?]))

(define-struct/contract assign ([dst (or/c symbol? mem?)]
                                [src (or/c s? mem?)]))
(define-struct/contract mathop ([op symbol?]
                                [larg t?]
                                [rarg t?]))
(define-struct/contract cmp ([destination (or/c symbol? null?)]
                             [comparator symbol?]
                             [larg t?]
                             [rarg t?]))
(define-struct/contract goto ([target symbol?]))
(define-struct/contract cjump ([larg t?]
                               [op symbol?]
                               [rarg t?]
                               [ttarget symbol?]
                               [ftarget symbol?]))
(define-struct/contract print ([val t?]))
(define-struct/contract allocate ([size t?]
                                  [init t?]))
(define-struct/contract array-error ([ptr t?]
                                     [index t?]))

(define l2instr? (or/c assign?
                       mathop?
                       cmp?
                       label?
                       goto?
                       cjump?
                       call?
                       tail-call?
                       return?
                       print?
                       allocate?
                       array-error?
                       mem?))

(define-struct/contract fun ([name label?]
                             [instrs (listof l2instr?)]))
(define-struct/contract prog ([funs (listof fun?)]))

(provide l2instr?
         (struct-out label)
         (struct-out mem)
         (struct-out return)
         (struct-out call)
         (struct-out tail-call)
         (struct-out assign)
         (struct-out mathop)
         (struct-out cmp)
         (struct-out goto)
         (struct-out cjump)
         (struct-out print)
         (struct-out allocate)
         (struct-out array-error)
         (struct-out fun)
         (struct-out prog))
