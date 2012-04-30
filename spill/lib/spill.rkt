#lang racket
(require "types.rkt")

(define dirty #f)
(define spilled 0)
(define spillct 0)
(define sprefix "deadbeef")
(define spillmap (make-hash))

(define (get-spill) spilled)

(define/contract (make-tvar prefix)
  (-> symbol? string?)
  (string-append
   (symbol->string prefix)
   (number->string spillct)))

(define/contract (make-tvar-kill/stack tvar offset)
  (-> symbol? integer? assign?)
  (make-tvar-kill tvar `ebp offset))

(define/contract (make-tvar-kill/mem tvar mm)
  (-> symbol? mem? assign?)
  (make-tvar-kill tvar (mem-addr mm) (mem-offset mm)))

(define/contract (make-tvar-kill tvar addr offset)
  (-> symbol? symbol? integer? assign?)
  (begin (let ([spillnum 
                (if dirty
                    (+ spillct 1)
                    spillct)])
           (set! dirty #f)
           (set! spillct (+ spillct 1))
           (assign tvar (mem addr offset)))))

(define/contract (make-tvar-gen tvar offset)
  (-> symbol? integer? assign?)
  (dirtify!)
  (assign (mem `ebp offset) tvar))

(define (dirtify!)
  (set! dirty #t))

(define (get-spill-bits)
  ;; FIXME: What contract works here?
  (let ([sval (get-spill)])
    (list (make-tvar sprefix)
          sval)))

(define/contract (spill code var offset prefix)
  (-> (listof l2instr?) symbol? number? symbol? (listof l2instr?))
  (begin
    (set! dirty #f)
    (set! spilled offset)
    (set! sprefix prefix)
    (filter (λ (x) (not (zilch? x))) (flatten (map
                                               (λ (x) (spill-instr x var))
                                               code)))))

(define/contract (spill-instr i v)
  (-> l2instr? symbol? (listof l2instr?))
  (cond
    [(assign? i) (spill-assign i v)]
    [(mathop? i) (spill-mathop i v)]
    [(cmp? i) (spill-cmp i v)]
    [(goto? i) (spill-goto i v)]
    [(cjump? i) (spill-cjump i v)]
    [(call? i) (spill-call i v)]
    [(tail-call? i) (spill-tail-call i v)]
    [(print? i) (spill-print i v)]
    [(allocate? i) (spill-allocate i v)]
    [(array-error? i) (spill-array-error i v)]
    [else (list i)]))

(define/contract (spill-assign i v)
  (-> assign? symbol? (listof l2instr?))
  (let ([dst (assign-dst i)]
        [src (assign-src i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (cond
        [(and (eq? src dst) (eq? src v)) (list)]
        [(and (mem? dst) (eq? (mem-addr dst) v))
         (list
          (make-tvar-kill/stack tvar offset)
          (assign (mem tvar (mem-offset dst)) (if (eq? src v)
                                                  (begin
                                                    (dirtify!)
                                                    tvar)
                                                  src)))]
        [(and (mem? src) (eq? (mem-addr src) v))
         (begin
           (dirtify!)
           (list
            (make-tvar-kill/stack tvar offset)
            (assign (if (eq? dst v)
                        tvar
                        dst)
                    (mem tvar (mem-offset src)))
            (if (eq? dst v)
                (make-tvar-gen tvar offset)
                (zilch))))]
        [(eq? dst v) (if (mem? src)
                         (list
                          (make-tvar-kill/mem tvar
                                              (if (eq? (mem-addr src) v)
                                                  (mem tvar (mem-offset src))
                                                  src))
                          (make-tvar-gen tvar offset))
                         (list
                          (assign (mem `ebp offset) src)))]
        [(eq? src v) (begin
                       (dirtify!)
                       (if (mem? dst)
                           (list
                            (make-tvar-kill/stack tvar offset)
                            (assign (if (eq? (mem-addr dst) v)
                                        (mem tvar (mem-offset dst))
                                        dst)
                                    tvar))
                           (list (assign dst (mem `ebp offset)))))]
        [else (list i)]))))

(define/contract (spill-mathop i v)
  (-> mathop? symbol? (listof l2instr?))
  (let ([op (mathop-op i)]
        [larg (mathop-larg i)]
        [rarg (mathop-rarg i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (cond
        [(eq? larg v) (list
                       (make-tvar-kill/stack tvar offset)
                       (mathop op tvar (if (eq? rarg v)
                                           tvar
                                           rarg))
                       (make-tvar-gen tvar offset))]
        [(eq? rarg v) (list
                       (make-tvar-kill/stack tvar offset)
                       (mathop op larg tvar))]
        [else (list i)]))))

(define/contract (spill-cmp i v)
  (-> cmp? symbol? (listof l2instr?))
  (let ([dest (cmp-destination i)]
        [cmptr (cmp-comparator i)]
        [larg (cmp-larg i)]
        [rarg (cmp-rarg i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])      
      (list
       (if (or (eq? larg v) (eq? rarg v))
           (make-tvar-kill/stack tvar offset)
           (zilch))
       (cmp (if (eq? dest v)
                tvar
                dest)
            cmptr
            (if (eq? larg v)
                tvar
                larg)
            (if (eq? rarg v)
                tvar
                rarg))
       (if (eq? dest v)
           (make-tvar-gen tvar offset)
           (zilch))))))


(define/contract (spill-goto i v)
  (-> goto? symbol? (listof l2instr?))
  (let ([target (goto-target i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (if (eq? target v)
          (list (make-tvar-kill/stack tvar offset)
                (goto tvar))
          (list i)))))

(define/contract (spill-cjump i v)
  (-> cjump? symbol? (listof l2instr?))
  (let ([larg (cjump-larg i)]
        [op (cjump-op i)]
        [rarg (cjump-rarg i)]
        [ttarg (cjump-ttarget i)]
        [ftarg (cjump-ftarget i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])      
      (list (if (foldl (λ (x y) (or (eq? x v) y))
                       #f
                       (list larg rarg ttarg ftarg))
                (make-tvar-kill/stack tvar offset)
                (zilch))
            (cjump (if (eq? larg v)
                       (begin
                         (dirtify!)
                         tvar)
                       larg)
                   op
                   (if (eq? rarg v)
                       (begin
                         (dirtify!)
                         tvar)
                       rarg)
                   (if (eq? ttarg v)
                       (begin
                         (dirtify!)
                         tvar)
                       ttarg)
                   (if (eq? ftarg v)
                       (begin
                         (dirtify!)
                         tvar)
                       ftarg))))))


(define/contract (spill-call i v)
  (-> call? symbol? (listof l2instr?))
  (let ([func (call-func i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (if (eq? func v)
          (list (make-tvar-kill/stack tvar offset)
                (call tvar))
          (list i)))))

(define/contract (spill-tail-call i v)
  (-> tail-call? symbol? (listof l2instr?))
  (let ([func (tail-call-func i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (if (eq? func v)
          (list (make-tvar-kill/stack tvar offset)
                (tail-call tvar))
          (list i)))))

(define/contract (spill-print i v)
  (-> print? symbol? (listof l2instr?))
  (let ([val (print-val i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (if (eq? val v)
          (list
           (make-tvar-kill/stack tvar offset)
           (print tvar))
          (list i)))))

(define/contract (spill-allocate i v)
  (-> allocate? symbol? (listof l2instr?))
  (let ([size (allocate-size i)]
        [init (allocate-init i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (if (or (eq? size v) (eq? init v))
          (list (make-tvar-kill/stack tvar offset)
                (allocate
                 (if (eq? size v)
                     tvar
                     size)
                 (if (eq? init v)
                     tvar
                     init)))
          (list i)))))

(define/contract (spill-array-error i v)
  (-> array-error? symbol? (listof l2instr?))
  (let ([ptr (array-error-ptr i)]
        [index (array-error-index i)]
        [bits (get-spill-bits)])
    (let ([tvar (string->symbol (car bits))]
          [offset (cadr bits)])
      (if (or (eq? ptr v) (eq? index v))
          (list (make-tvar-kill/stack tvar offset)
                (array-error
                 (if (eq? ptr v)
                     tvar
                     ptr)
                 (if (eq? index v)
                     tvar
                     index)))
          (list i)))))

(provide spill)
