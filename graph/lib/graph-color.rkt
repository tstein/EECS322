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


(define/contract (find-inout-conflicts inouts var)
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


(define/contract (find-kill-out-conflicts kills outs var)
  (-> (listof (listof symbol?)) (listof set?) symbol? set?)
  (define koconflicts (set))
  (for/list ([k (map list->set kills)]
             [o outs])
    (if (set-member? k var)
        (set! koconflicts (set-union koconflicts o))
        null))
  (set-remove koconflicts var))


(define/contract (graph-color f)
  (-> fun? coloring?)
  (define inouts (liveness f))
  (define allvars (set-subtract (all-vars f)
                                register-set
                                (set 'ebp 'esp)))
  (define conflicts (make-hash))
  ;; all registers conflict with all others
  (for/list ([r registers])
    (hash-set! conflicts r (filter-out-sym registers r)))
  ;; set up conflicts for other vars
  (for/list ([v allvars])
    (if (not (hash-has-key? conflicts v))
        (hash-set! conflicts v (list))
        null))
  (for/list ([v allvars])
    (if (set-member? register-set v)
        null
        (let ([io-conflicts (find-inout-conflicts inouts v)]
              [ko-conflicts (find-kill-out-conflicts (map kill (fun-instrs f))
                                                     (inout-sets-outs inouts)
                                                     v)])
          (let ([all-conflicts (set->list (set-union
                                           (list->set io-conflicts)
                                           ko-conflicts))])
            (hash-set! conflicts v all-conflicts)
            (for/list ([vc all-conflicts])
              (hash-set!
               conflicts
               vc
               (sort
                (cons v (hash-ref conflicts vc))
                symbol<?)))))))
  (for/list ([v (hash-keys conflicts)])
    (hash-set! conflicts v (sort
                            (set->list
                             (list->set
                              (hash-ref conflicts v)))
                            symbol<?)))
  (define pre-mapping-conflicts (hash-copy conflicts))
  ;; actually do the mapping
  (define mappings (make-hash))
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
;(define myfun (fun (label 'f) instrs));
;
;(graph-color myfun)
