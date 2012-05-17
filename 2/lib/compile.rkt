#lang racket
(require srfi/13
         "graph-color.rkt"
         "spill.rkt"
         "types.rkt"
         "parse.rkt")



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
  p)


(provide compile)
