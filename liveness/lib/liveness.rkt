#lang racket
(require srfi/1
         "types.rkt")

(define-struct/contract inout-sets
  ([ins  (listof list?)]
   [outs (listof list?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(~a ~a)"
             (cons 'in  (inout-sets-ins  v))
             (cons 'out (inout-sets-outs v)))))


;; utility
(define/contract (init-inout-sets len)
  (-> integer? inout-sets?)
  (define/contract (init-inout-sets/inner len sets)
    (-> integer? inout-sets? inout-sets?)
    (if (= len 0)
        sets
        (let ([ins  (cons (list) (inout-sets-ins  sets))]
              [outs (cons (list) (inout-sets-outs sets))])
          (init-inout-sets/inner (- len 1) (inout-sets ins outs)))))
  (init-inout-sets/inner len (inout-sets '() '())))

(define/contract (filter-non-vars l)
  (-> list? (listof symbol?))
  (filter symbol? l))


;; liveness
(define/contract (liveness f)
  ;; FIXME: This contract is way too relaxed.
  (-> fun? inout-sets?)
  (define raw-instrs (fun-instrs f))
  (define numinstrs (length raw-instrs))
  (define instrs (for/list ([i (in-range 0 numinstrs)]
                            [j raw-instrs])
                   (cons i j)))
  (define inouts (init-inout-sets numinstrs))
  (inout-sets (list '(x)) (list '(y))))


;; gen
(define/contract (gen i)
  (-> l2instr? (listof symbol?))
  (filter-non-vars
   (cond
     [(assign? i)      (list (assign-src i))]
     [(mathop? i)      (list (mathop-larg i)
                             (mathop-rarg i))]
     [(cmp? i)         (list (cmp-larg i)
                             (cmp-rarg i))]
     [(goto? i)        (list (goto-target i))]
     [(cjump? i)       (list (cjump-larg i)
                             (cjump-rarg i))]
     [(call? i)        (list (call-func i)
                             'eax 'edx 'ecx)]
     [(tail-call? i)   (list (tail-call-func i)
                             'eax 'edx 'ecx
                             'esi 'edi)]
     [(return? i)      (list 'eax
                             'esi 'edi)]
     [(print? i)       (list (print-val i))]
     [(allocate? i)    (list (allocate-size i)
                             (allocate-init i))]
     [(array-error? i) (list (array-error-ptr i)
                             (array-error-index i))]
     [else '()])))


;; kill
(define/contract (kill i)
  (-> l2instr? (listof symbol?))
  (filter-non-vars
   (cond
     [(assign? i)           (list (assign-dst i))]
     [(mathop? i)           (list (mathop-larg i))]
     [(cmp? i)              (list (cmp-larg i))]
     [(call? i)             (list 'eax 'ebx 'ecx 'edx)]
     [(or (return? i)
          (print? i)
          (allocate? i)
          (array-error? i)) (list 'eax)]
     [else '()])))


(provide liveness)
