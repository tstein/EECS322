#lang racket
(require srfi/1
         "types.rkt")

(define-struct/contract inout-sets
  ([ins  (listof set?)]
   [outs (listof set?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(~a ~a)"
             (cons 'in
                   (map symbol-set->sorted-list (inout-sets-ins  v)))
             (cons 'out
                   (map symbol-set->sorted-list (inout-sets-outs v))))))


;; utility
(define/contract (init-inout-sets len)
  (-> integer? inout-sets?)
  (define/contract (init-inout-sets/inner len sets)
    (-> integer? inout-sets? inout-sets?)
    (if (= len 0)
        sets
        (let ([ins  (cons (set) (inout-sets-ins  sets))]
              [outs (cons (set) (inout-sets-outs sets))])
          (init-inout-sets/inner (- len 1) (inout-sets ins outs)))))
  (init-inout-sets/inner len (inout-sets '() '())))


(define/contract (symbol-set->sorted-list syms)
  (-> set? (listof symbol?))
  (sort (set->list syms)
        (λ (s o)
          (string<?
           (symbol->string s)
           (symbol->string o)))))

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
  (inout-sets (list (set 'x)) (list (set 'y))))


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
