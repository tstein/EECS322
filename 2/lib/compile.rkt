#lang racket
(require srfi/13
         "graph-color.rkt"
         "spill.rkt"
         "types.rkt"
         "parse.rkt")


(define/contract (make-coloring-map clrng)
  (-> coloring? hash?)
  (define cmap (make-hash))
  (for/list ([c (coloring-colors clrng)])
    (hash-set! cmap (car c) (cadr c)))
  cmap)


(define/contract (compile-fun f)
  (-> fun? fun?)
  (define clrng (graph-color f))
  (if (false? (coloring-colors clrng))
      (let ([name   (fun-name   f)]
            [instrs (fun-instrs f)]
            [to-spill (car (most-conflicted-vars (coloring-adjacency clrng)))])
        (compile-fun (fun name
                          (spill instrs to-spill -4 `_splt))))
      (let ([cmap (make-coloring-map clrng)])
        (color-fun f cmap))))


(define/contract (color-fun f cmap)
  (-> fun? hash? fun?)
  (fun (fun-name f)
       (filter (λ (x)
                 (not (zilch? x)))
               (map (λ (i) (color-instr i cmap)) (fun-instrs f)))))


(define/contract (color-instr i cmap)
  (-> l2instr? hash? l2instr?)
  (cond
    [(assign? i) (let ([new-assign (let ([dst (assign-dst i)]
                                         [src (assign-src i)])
                                     (assign ((if (mem? dst)
                                                  color-mem
                                                  color-sym) dst cmap)
                                             ((if (mem? src)
                                                  color-mem
                                                  color-sym) src cmap)))])
                   (if (eq? (assign-src new-assign)
                            (assign-dst new-assign))
                       (zilch)
                       new-assign))]
    [(mathop? i) (mathop (mathop-op i)
                         (color-sym (mathop-larg i) cmap)
                         (color-sym (mathop-rarg i) cmap))]
    [(cmp? i) (cmp (color-sym (cmp-destination i) cmap)
                   (cmp-comparator i)
                   (color-sym (cmp-larg i) cmap)
                   (color-sym (cmp-rarg i) cmap))]
    [(goto? i) (goto (color-sym (goto-target i) cmap))]
    [(cjump? i) (cjump (color-sym (cjump-larg i) cmap)
                       (cjump-op i)
                       (color-sym (cjump-rarg i) cmap)
                       (color-sym (cjump-ttarget i) cmap)
                       (color-sym (cjump-ftarget i) cmap))]
    [(call? i) (call (color-sym (call-func i) cmap))]
    [(tail-call? i) (tail-call (color-sym (tail-call-func i) cmap))]
    [(print? i) (print (color-sym (print-val i) cmap))]
    [(allocate? i) (allocate (color-sym (allocate-size i) cmap)
                             (color-sym (allocate-init i) cmap))]
    [(array-error? i) (array-error (color-sym (array-error-ptr i) cmap)
                                   (color-sym (array-error-index i) cmap))]
    [(or (label? i)
         (return? i)) i]))


(define/contract (color-mem m cmap)
  (-> mem? hash? mem?)
  (mem (color-sym (mem-addr m) cmap)
       (color-sym (mem-offset m) cmap)))


(define/contract (color-sym s cmap)
  (-> (or/c symbol? integer?) hash? (or/c symbol? integer?))
  (if (hash-has-key? cmap s)
      (hash-ref cmap s)
      s))

;                                                   
;                                                   
;                                                   
;                                                   
;                                        ;          
;     ;                    ;    ;  ;     ;      ;   
;    ; ;   ;   ;  ;;;;;   ; ;;   ;; ;  ;;;;;   ; ;; 
;   ;   ;   ; ;   ;   ;  ;   ;   ;       ;    ;     
;   ;;;;;    ;    ;   ;; ;   ;;  ;      ;      ;;   
;   ;        ;    ;   ;  ;   ;   ;      ;        ;; 
;   ;       ; ;   ;   ;  ;   ;   ;      ;     ;   ; 
;    ;;;;  ;   ;  ;;;;    ;;;    ;       ;;;   ;;;  
;                 ;             ;                   
;                 ;                                 
;                                                   

(define/contract (compile p)
  (-> prog? prog?)
  (prog (map compile-fun (prog-funs p))))


(provide compile)
