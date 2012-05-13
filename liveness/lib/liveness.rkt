#lang racket
(require srfi/1
         "types.rkt")


(define-struct/contract inout-sets
  ([ins  (listof set?)]
   [outs (listof set?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "(~a\n~a)"
             (cons 'in
                   (map symbol-set->sorted-list (inout-sets-ins  v)))
             (cons 'out
                   (map symbol-set->sorted-list (inout-sets-outs v))))))


(define/contract (inout-sets-eq? one other)
  (-> inout-sets? inout-sets? boolean?)
  (define (listofset-eq? one other)
    (foldl (λ (fst snd last)
             (and last
                  (set=? fst snd)))
           #t
           one
           other))
  (and
   (= (length (inout-sets-ins one))
      (length (inout-sets-ins other)))
   (= (length (inout-sets-outs one))
      (length (inout-sets-outs other)))
   (listofset-eq?
    (inout-sets-ins one)
    (inout-sets-ins other))
   (listofset-eq?
    (inout-sets-outs one)
    (inout-sets-outs other))))


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


(define/contract (get-instr instrs n)
  (-> list? integer? l2instr?)
  (cdar (filter (λ (i) (eq? n (car i))) instrs)))


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
  (-> fun? inout-sets?)
  (define raw-instrs (fun-instrs f))
  (define numinstrs (length raw-instrs))
  (define instrs (for/list ([i (in-range 0 numinstrs)]
                            [j raw-instrs])
                   (cons i j)))
  (define/contract (liveness/inner inouts)
    (-> inout-sets? inout-sets?)
    (define old-inouts inouts)
    (define old-ins  (inout-sets-ins  old-inouts))
    (define old-outs (inout-sets-outs old-inouts))
    (define ins  (list))
    (define outs (list))
    (for/list ([i (in-range 0 numinstrs)])
      (define thisinstr (get-instr instrs i))
      (define new-ins (set-union (list->set (gen thisinstr))
                                 (set-subtract (list-ref old-outs i)
                                               (list->set (kill thisinstr)))))
      (define new-outs (if (< i (- numinstrs 1))
                           (list-ref old-ins (+ i 1))
                           (set)))
      (set! ins  (reverse (cons new-ins  (reverse ins))))
      (set! outs (reverse (cons new-outs (reverse outs)))))
    (let ([inouts (inout-sets ins outs)])
      (if (inout-sets-eq? inouts old-inouts)
          inouts
          (liveness/inner inouts))))
  (liveness/inner (init-inout-sets numinstrs)))


;; gen
(define/contract (gen i)
  (-> l2instr? (listof symbol?))
  (filter-non-vars
   (cond
     [(assign? i)      (list (assign-src i)
                             (if (mem? (assign-dst i))
                                 (mem-addr (assign-dst i))
                                 (zilch)))]
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
     [(cmp? i)              (list (cmp-destination i)
                                  (cmp-larg i))]
     [(call? i)             (list 'eax 'ebx 'ecx 'edx)]
     [(or (return? i)
          (print? i)
          (allocate? i)
          (array-error? i)) (list 'eax)]
     [else '()])))


(provide liveness)

;(define myfun (fun (label 'f) (list
;                               (assign 'x2 'eax)
;                               (mathop '*= 'x2 'x2)
;                               (assign '2x2 'x2)
;                               (mathop '*= '2x2 2)
;                               (assign '3x 'eax)
;                               (mathop '*= '3x 3)
;                               (assign 'eax '2x2)
;                               (mathop '+= 'eax '3x)
;                               (mathop '+= 'eax 4)
;                               (return))))

;(liveness myfun)