#lang racket
(require racket/contract)

(define-struct/contract label ([name symbol?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "~s" (label-name v))))

(define-struct/contract mem ([addr symbol?]
                             [offset integer?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(mem ~s ~s)"
             (mem-addr   v)
             (mem-offset v))))


(define s? (or/c symbol?
                 integer?
                 label?))
(define t? (or/c symbol?
                 integer?))
(define u? (or/c symbol?
                 label?))

(define-struct/contract return ()
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p "(return)")))

(define-struct/contract call ([func u?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(call ~s)"
             (call-func v))))

(define-struct/contract tail-call ([func u?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(tail-call ~s)"
             (tail-call-func v))))

(define-struct/contract assign ([dst (or/c symbol? mem?)]
                                [src (or/c s? mem?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(~s <- ~s)"
             (assign-dst v)
             (assign-src v))))

(define-struct/contract mathop ([op symbol?]
                                [larg t?]
                                [rarg t?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(~s ~s ~s)"
             (mathop-larg v)
             (mathop-op   v)
             (mathop-rarg v))))

(define-struct/contract cmp ([destination (or/c symbol? null?)]
                             [comparator symbol?]
                             [larg t?]
                             [rarg t?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(~s <- ~s ~s ~s)"
             (cmp-destination v)
             (cmp-larg        v)
             (cmp-comparator  v)
             (cmp-rarg        v))))

(define-struct/contract goto ([target symbol?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(goto ~s)"
             (goto-target v))))

(define-struct/contract cjump ([larg t?]
                               [op symbol?]
                               [rarg t?]
                               [ttarget symbol?]
                               [ftarget symbol?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(cjump ~s ~s ~s ~s ~s)"
             (cjump-larg    v)
             (cjump-op      v)
             (cjump-rarg    v)
             (cjump-ttarget v)
             (cjump-ftarget v))))

(define-struct/contract print ([val t?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(eax <- (print ~s))"
             (print-val v))))

(define-struct/contract allocate ([size t?]
                                  [init t?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(eax <- (allocate ~s ~s))"
             (allocate-size v)
             (allocate-init v))))

(define-struct/contract array-error ([ptr t?]
                                     [index t?])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(eax <- (array-error ~s ~s))"
             (array-error-ptr   v)
             (array-error-index v))))

(define-struct/contract zilch ()
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p "<zilch>")))

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
                       mem?
                       zilch?))

(define-struct/contract fun ([name label?]
                             [instrs (listof l2instr?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p " (")
    (fprintf p "~s" (fun-name v))
    (for/list ([i (fun-instrs v)])
      (fprintf p "\n  ~s" i))
    (fprintf p " \n )")))

(define-struct/contract prog ([funs (listof fun?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p "(")
    (for/list ([f (prog-funs v)])
      (fprintf p "\n~s" f))
    (fprintf p "\n)\n")))

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
         (struct-out zilch)
         (struct-out fun)
         (struct-out prog))
