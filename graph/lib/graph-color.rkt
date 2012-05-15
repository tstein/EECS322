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
(define/contract (register? var)
  (-> symbol? boolean?)
  (set-member? register-set var))

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
  ;; FIXME: this list will be incomplete
  (define allvars (foldl
                   set-union
                   (set)
                   (append
                    (inout-sets-ins  inouts)
                    (inout-sets-outs inouts))))
  (define conflicts (make-hash))
  ;; all registers conflict with all others
  (for/list ([r registers])
    (hash-set! conflicts r (filter-out-sym registers r)))
  ;; set up conflicts for other vars
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
  (define pre-mapping-conflicts (hash-copy conflicts))
  ;; actually do the mapping
  (define mappings (make-hash))
  ;(display (most-conflicted-vars conflicts))
  ;(display "\n")
  (for/list ([cv (most-conflicted-vars conflicts)])
    ;; start with all regs
    (define candidates register-set)
    ;(display (format "allregs: ~a\n" candidates))
    ;; remove those with direct conflicts
    ;(display (format "directs: ~a\n" (list->set (hash-ref conflicts cv))))
    (set! candidates (set-subtract candidates (list->set (hash-ref conflicts cv))))
    ;(display (format "minus directs: ~a\n" candidates))
    (for/list ([ov (hash-ref conflicts cv)])
      ;; if a non-register that's been mapped is a conflict, remove the register
      ;; it's mapped to
      (if (set-member? register-set ov)
          null
          (if (hash-has-key? mappings ov)
              (set! candidates (set-remove candidates (hash-ref mappings ov)))
              null))
      ;(display (format "minus already mappeds ~a\n" candidates))
      )
    (if (set-empty? candidates)
        (hash-set! mappings cv null)
        (let ([mappedto (car (set->list candidates))])
          (hash-set! mappings cv mappedto)
          (hash-set! conflicts cv (sort (cons mappedto (hash-ref conflicts cv))
                                        symbol<?))))
    ;(display (format "mappings: ~a\n" mappings))
    )
  (define colors
    (if (not (null?
              (filter
               null?
               (hash-values mappings))))
        #f
        (let ([vars (reverse (sort (hash-keys mappings) symbol<?))])
          (foldl (λ (v lst)
                   (cons (list v (hash-ref mappings v)) lst))
                 '()
                 vars))))
  (coloring (map (λ (x)
                   (cons x (hash-ref pre-mapping-conflicts x)))
                 (sort (hash-keys pre-mapping-conflicts) symbol<?))
            colors))

(define/contract (most-conflicted-vars conflicts)
  (-> hash? (listof symbol?))
  (define
    vars-by-conflicts
    (map (λ (v)
           (cons v (length (hash-ref conflicts v))))
         (hash-keys conflicts)))
  (filter
   (λ (v) (not (register? v)))
   (map car (sort vars-by-conflicts (λ (one other)
                                      (> (cdr one) (cdr other)))))))


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
