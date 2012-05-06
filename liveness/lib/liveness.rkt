#lang racket
(require srfi/1
         "types.rkt")

;; utility
(define/contract (list-o-lists len)
  (-> integer? (listof null?))
  (define/contract (list-o-lists/inner len lists)
    (-> integer? (listof null?) (listof null?))
    (if (= len 0)
        '()
        (cons '() (list-o-lists/inner (- len 1) lists))))
  (list-o-lists/inner len '()))

(define/contract (filter-non-vars l)
  (-> list? (listof symbol?))
  (filter symbol? l))


;; liveness
(define/contract (liveness instrs)
  (-> (listof l2instr?) (listof (listof symbol?)))
  (define numinstrs (length instrs))
  (define instrs (for/list ([i (in-range 0 numinstrs)]
                            [j instrs])
                   (cons i j)))
  (define ins (list-o-lists numinstrs))
  (define outs (list-o-lists numinstrs))
  '('('in '('x)) '('out '('y))))


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
