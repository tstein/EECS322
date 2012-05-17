#lang racket
(require srfi/13
         "liveness.rkt"
         "types.rkt")


;                                                   
;                                                   
;                                                   
;                   ;    ;;;      ;                 
;            ;             ;             ;          
;            ;             ;             ;         ;
;   ;   ;  ;;;;;   ;;      ;     ;;    ;;;;;  ;   ; 
;   ;   ;    ;      ;      ;      ;      ;     ;  ; 
;   ;   ;   ;       ;      ;      ;     ;      ;  ; 
;   ;   ;   ;       ;      ;      ;     ;      ; ;  
;   ;   ;   ;       ;      ;      ;     ;       ;;  
;    ;;;;    ;;;   ;;;;  ;;;;;   ;;;;    ;;;    ;   
;                                               ;   
;                                            ;;;    
;                                                   
(define/contract (symbol<? one other)
  (-> symbol? symbol? boolean?)
  (string<? (symbol->string one)
            (symbol->string other)))


(define/contract (filter-out-sym lst sym)
  (-> (listof symbol?) symbol? (listof symbol?))
  (filter (λ (x) (not (eq? x sym))) lst))



;                                                   
;                                                   
;                                                   
;   ;        ;                                      
;   ;      ;; ;;                                    
;   ;          ;                  ;     ;  ;        
;   ;          ;          ;;;;   ; ;;  ; ;; ; ;;;;; 
;   ;         ;          ;   ;  ;   ;  ; ;  ; ;   ; 
;   ;        ;           ;      ;   ;; ; ;  ; ;   ;;
;   ;       ;            ;      ;   ;  ; ;  ; ;   ; 
;   ;       ;            ;   ;  ;   ;  ; ;  ; ;   ; 
;   ;;;;;  ;;;;;          ;;;;   ;;;   ; ;  ; ;;;;  
;                                             ;     
;                                             ;     
;                                                   
(define/contract (all-vars x)
  (-> (or/c fun? l2instr?) set?)
  (list->set
   (filter
    (λ (s)
      (and (symbol? s)
           (not (string-prefix? ":" (symbol->string s)))))
    (set->list
     (cond
       [(fun? x) (foldl set-union (set) (map all-vars (fun-instrs x)))]
       [(assign? x)      (set (if (mem? (assign-dst x))
                                  (mem-addr (assign-dst x))
                                  (assign-dst x))
                              (if (mem? (assign-src x))
                                  (mem-addr (assign-src x))
                                  (assign-src x)))]
       [(mathop? x)      (set (mathop-larg x)
                              (mathop-rarg x))]
       [(cmp? x)         (set (cmp-destination x)
                              (cmp-larg x)
                              (cmp-rarg x))]
       [(goto? x)        (set (goto-target x))]
       [(cjump? x)       (set (cjump-larg x)
                              (cjump-rarg x)
                              (cjump-ttarget x)
                              (cjump-ftarget x))]
       [(call? x)        (set (call-func x))]
       [(tail-call? x)   (set (tail-call-func x))]
       [(print? x)       (set 'eax
                              (print-val x))]
       [(allocate? x)    (set 'eax
                              (allocate-size x)
                              (allocate-init x))]
       [(array-error? x) (set 'eax
                              (array-error-ptr   x)
                              (array-error-index x))]
       [(or (label? x)
            (return? x)) (set)])))))



;                                                          
;                                                          
;                                                          
;                 ;;;                    ;                 
;                   ;                                      
;            ;      ;      ;    ;  ;             ;     ;  ;
;    ;;;;   ; ;;    ;     ; ;;   ;; ;   ;;    ;;; ;  ;; ;; 
;   ;   ;  ;   ;    ;    ;   ;   ;       ;    ;   ;  ;   ; 
;   ;      ;   ;;   ;    ;   ;;  ;       ;    ;   ;  ;  ;  
;   ;      ;   ;    ;    ;   ;   ;       ;    ;   ;   ;;   
;   ;   ;  ;   ;    ;    ;   ;   ;       ;    ;   ;  ;     
;    ;;;;   ;;;   ;;;;;   ;;;    ;      ;;;;  ;   ;   ;;;; 
;                               ;                    ;   ; 
;                                                    ;; ;; 
;                                                      ;   
(define registers (list 'eax 'ebx 'ecx 'edi 'edx 'esi))
(define register-set (list->set registers))
(define caller-save-set (set 'eax 'ebx 'ecx 'edx))
(define/contract (register? var)
  (-> symbol? boolean?)
  (set-member? register-set var))


(define-struct/contract coloring
  ([adjacency (listof (listof symbol?))]
   [colors    (or/c (listof (listof symbol?))
                    false?)])
  #:property prop:custom-write
  (λ (v p w?)
    (pretty-display
     (coloring-adjacency v)
     p)
    (pretty-display
     (coloring-colors v)
     p)))


(define/contract (find-inout-conflicts inouts instrs var)
  (-> inout-sets? (listof l2instr?) symbol? (listof symbol?))
  (sort
   (filter-out-sym
    (set->list
     (set-union
      ;; variables in the first in set conflict
      (let ([firstins (car (inout-sets-ins inouts))])
        (if (set-member? firstins var)
            firstins
            (set)))
      ;; variables in any out set together conflict
      (foldl
       set-union
       (set)
       (map
        (λ (outset instr)
          (if (set-member? outset var)
              (if (assign? instr)
                  (set-subtract outset 
                                (set (assign-dst instr)
                                     (assign-src instr)))
                  outset)
              (set)))
        (inout-sets-outs inouts)
        instrs))))
    var)
   symbol<?))


;; If var appears in an instruction's kill set, it conflicts with that
;; instr's out set.
(define/contract (find-kill-out-conflicts instrs outs var)
  (-> (listof l2instr?) (listof set?) symbol? set?)
  (define kills (map kill instrs))
  (define koconflicts (set))
  (for/list ([i instrs]
             [k (map list->set kills)]
             [o outs])
    (if (set-member? k var)
        (let ([newconflicts 
               ;; broken?
               (if (assign? i)
                   (set-remove o (assign-src i))
                   o)])
          (set! koconflicts (set-union koconflicts newconflicts)))
        null))
  (set-remove koconflicts var))


;; There are a few stupid special cases.
(define/contract (find-special-case-conflicts instrs var)
  (-> (listof l2instr?) symbol? set?)
  (define scconflicts (set))
  (for/list ([i instrs])
    (cond
      [(cmp? i) (if (eq? (cmp-destination i) var)
                    (set! scconflicts (set-union scconflicts
                                                 (set 'edi 'esi)))
                    null)]
      [(and (mathop? i)
            (or (eq? (mathop-op i) '<<=)
                (eq? (mathop-op i) '>>=))) (if (eq? (mathop-rarg i) var)
                                               (set! scconflicts
                                                     (set-union scconflicts
                                                                (set 'eax 'ebx 'edx 'edi 'esi)))
                                               null)]
      [else null]))
  scconflicts)


(define/contract (add-regs-to-inout-sets inouts)
  (-> inout-sets? inout-sets?)
  (define ins  (inout-sets-ins  inouts))
  (define outs (inout-sets-outs inouts))
  (define first-ins  (car ins))
  (define first-outs (car outs))
  (inout-sets (cons (set-union first-ins
                               caller-save-set))
              outs))


(define/contract (graph-color f)
  (-> fun? coloring?)
  (define instrs (fun-instrs f))
  (define inouts-raw (liveness f))
  (define inouts (inout-sets (inout-sets-ins  inouts-raw)
                             (inout-sets-outs inouts-raw)))
  (define allvars (set-subtract (set-union register-set
                                           (all-vars f))
                                (set 'ebp 'esp)))
  (define conflicts (make-hash))
  (define pre-mapping-conflicts null)
  (define mappings (make-hash))
  ;; all registers conflict with all others
  (for/list ([r registers])
    (hash-set! conflicts r (filter-out-sym registers r)))
  ;; handle empty programs explicitly
  (unless (null? instrs)
    (begin
      ;; set up conflicts for other vars
      (for/list ([v allvars])
        (if (not (hash-has-key? conflicts v))
            (hash-set! conflicts v (list))
            null))
      (for/list ([v allvars])
        (let ([io-conflicts (find-inout-conflicts inouts instrs v)]
              [ko-conflicts (find-kill-out-conflicts instrs
                                                     (inout-sets-outs inouts)
                                                     v)]
              [firstin-conflicts (if (and (not (set-member? register-set v))
                                          (set-member? (list->set (gen (car (fun-instrs f))))
                                                       v))
                                     (set 'eax 'ebx 'edx 'esi 'edi)
                                     (set))]
              [sc-conflicts (find-special-case-conflicts instrs v)])
          (let ([all-conflicts (set->list
                                (set-subtract
                                 (set-union
                                  (list->set (hash-ref conflicts v))
                                  (list->set io-conflicts)
                                  ko-conflicts
                                  sc-conflicts)
                                 ;firstin-conflicts)
                                 (set 'ebp 'esp)))])
            (hash-set! conflicts v all-conflicts)
            (for/list ([vc all-conflicts])
              (hash-set!
               conflicts
               vc
               (sort
                (cons v (hash-ref conflicts vc))
                symbol<?))))))
      (for/list ([v (hash-keys conflicts)])
        (hash-set! conflicts v (sort
                                (set->list
                                 (list->set
                                  (hash-ref conflicts v)))
                                symbol<?)))))
  (set! pre-mapping-conflicts (hash-copy conflicts))
  ;; actually do the mapping
  (for/list ([cv (most-conflicted-vars conflicts)])
    ;; start with all regs
    (define candidates register-set)
    ;; remove those with direct conflicts
    (set! candidates
          (set-subtract candidates (list->set (hash-ref conflicts cv))))
    (for/list ([ov (hash-ref conflicts cv)])
      ;; if a non-register that's been mapped is a conflict, remove the register
      ;; it's mapped to
      (if (set-member? register-set ov)
          null
          (if (hash-has-key? mappings ov)
              (set! candidates (set-remove candidates (hash-ref mappings ov)))
              null)))
    (if (set-empty? candidates)
        (hash-set! mappings cv null)
        (let ([mappedto (car (set->list candidates))])
          (hash-set! mappings cv mappedto)
          (hash-set! conflicts cv (sort (cons mappedto (hash-ref conflicts cv))
                                        symbol<?)))))
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


(provide graph-color
         most-conflicted-vars
         (struct-out coloring))
