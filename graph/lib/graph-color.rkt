#lang racket
(require "liveness.rkt"
         "types.rkt")


; utility
(define/contract (symbol<? one other)
  (-> symbol? symbol? boolean?)
  (string<? (symbol->string one)
            (symbol->string other)))

(define/contract (filter-out-sym lst sym)
  (-> (listof symbol?) symbol? (listof symbol?))
  (filter (λ (x) (not (eq? x sym))) lst))


; coloring
(define registers (list 'eax 'ebx 'ecx 'edi 'edx 'esi))
(define register-set (list->set registers))

(define-struct/contract coloring
  ([adjacency (listof (listof symbol?))]
   [colors    (or/c (listof (listof symbol?))
                    false?)])
  #:property prop:custom-write
  (λ (v p w?)
    (fprintf p
             "~a\n~a"
             (coloring-adjacency v)
             (coloring-colors v))))


(define/contract (find-conflicts inouts var)
  (-> inout-sets? symbol? (listof symbol?))
  (sort
   (filter-out-sym
    (set->list
     (set-union
      (let ([firstins (car (inout-sets-ins inouts))])
        (if (set-member? firstins var)
            firstins
            (set)))
      (foldl
       set-union
       (set)
       (map
        (λ (outset)
          (if (set-member? outset var)
              outset
              (set)))
        (inout-sets-outs inouts)))))
    var)
   symbol<?))


(define/contract (graph-color f)
  (-> fun? coloring?)
  (define inouts (liveness f))
  ;; this list will be incomplete
  (define allvars (foldl
                   set-union
                   (set)
                   (append
                    (inout-sets-ins  inouts)
                    (inout-sets-outs inouts))))
  (define conflicts (make-hash))
  (for/list ([r registers])
    (hash-set! conflicts r (filter-out-sym registers r)))
  (for/list ([v allvars])
    (if (set-member? register-set v)
        null
        (let ([var-conflicts (find-conflicts inouts v)])
          (hash-set! conflicts v var-conflicts)
          (for/list ([vc var-conflicts])
            (if (set-member? register-set vc)
                (hash-set!
                 conflicts
                 vc
                 (sort
                  (cons v (hash-ref conflicts vc))
                  symbol<?))
                null)))))
  (coloring (map (λ (x)
                   (cons x (hash-ref conflicts x)))
                 (sort (hash-keys conflicts) symbol<?))
            #f))

(provide graph-color)


;(define instrs (list
;                (assign 'rx 'eax)
;                (mathop '+= 'rx 'ebx)
;                (mathop '+= 'rx 'ecx)
;                (mathop '+= 'rx 'edx)
;                (mathop '+= 'rx 'esi)
;                (mathop '+= 'rx 'edi)
;                (mathop '+= 'rx 'eax)))
;
;(define myfun (fun (label 'f) instrs))
;
;(graph-color myfun)
