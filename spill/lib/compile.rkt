#lang racket
(require srfi/13
         "types.rkt"
         "parse.rkt")

(define/contract (parens s)
  (string? . -> . string?)
  (string-append "(" s ")"))

(define/contract (compileLabel lab)
  (label? . -> . string?)
  (symbol->string (label-name lab)))

(define/contract (compileArg arg)
  (-> (or/c symbol? integer? label? mem?) string?)
  (cond
    [(symbol? arg) (symbol->string arg)]
    [(integer? arg) (number->string arg)]
    [(label? arg) (compileLabel arg)]
    [(mem? arg) (parens (string-join
                         (list
                          "mem"
                          (symbol->string (mem-addr arg))
                          (number->string (mem-offset arg)))
                         " "))]))

(define/contract (compileReturn _)
  (return? . -> . string?)
  "(return)")

(define/contract (compileCall kall)
  (call? . -> . string?)
  (parens
   (string-append
    "call "
    (compileArg (call-func kall)))))

(define (compileTailCall tcall)
  (parens
   (string-append
    "tail-call "
    (compileArg (tail-call-func tcall)))))

(define/contract (compileAssign ass)
  (assign? . -> . string?)
  (parens (string-append
           (compileArg (assign-dst ass))
           " <- "
           (compileArg (assign-src ass)))))

(define/contract (compileMathop mthp)
  (mathop? . -> . string?)
  (let ([op (mathop-op mthp)]
        [larg (mathop-larg mthp)]
        [rarg (mathop-rarg mthp)])
    (parens (string-append
             (compileArg larg) " "
             (symbol->string op) " "
             (compileArg rarg)))))
             

(define/contract (compileCmp comp)
  (cmp? . -> . string?)
  (let ([dest (cmp-destination comp)]
        [cmptr (cmp-comparator comp)]
        [larg (cmp-larg comp)]
        [rarg (cmp-rarg comp)])
    (parens (string-append
             (compileArg dest) " <- "
             (compileArg larg) " "
             (symbol->string cmptr) " "
             (compileArg rarg)))))
             

(define/contract (compileGoto gt)
  (goto? . -> . string?)
  (parens (string-append
           "goto "
           (symbol->string (goto-target gt)))))

(define/contract (compileCjump cjmp)
  (cjump? . -> . string?)
  (let ([larg (cjump-larg cjmp)]
        [op (cjump-op cjmp)]
        [rarg (cjump-rarg cjmp)]
        [ttarg (cjump-ttarget cjmp)]
        [ftarg (cjump-ftarget cjmp)])
  (parens (string-append
           "cjump "
           (compileArg larg) " "
           (symbol->string op) " "
           (compileArg rarg) " "
           (compileArg ttarg) " "
           (compileArg ftarg)))))
           
           

(define/contract (compilePrint prnt)
  (print? . -> . string?)
  (parens (string-append
           "eax <- (print "
           (compileArg (print-val prnt)) ")")))

(define/contract (compileAllocate alloc)
  (allocate? . -> . string?)
  (let ([size (allocate-size alloc)]
        [init (allocate-init alloc)])
  (parens (string-append
           "eax <- (allocate "
           (compileArg size) " "
           (compileArg init) ")"))))

(define/contract (compileArrayError arrerr)
  (array-error? . -> . string?)
  (let ([ptr (array-error-ptr arrerr)]
        [index (array-error-index arrerr)])
  (parens (string-append
           "eax <- (array-error "
           (compileArg ptr) " "
           (compileArg index) ")"))))

(define/contract (compileInstr instr)
  (l2instr? . -> . string?)
  (let ([compfun
         (cond
           [(label? instr) compileLabel]
           [(return? instr) compileReturn]
           [(call? instr) compileCall]
           [(tail-call? instr) compileTailCall]
           [(assign? instr) compileAssign]
           [(mathop? instr) compileMathop]
           [(cmp? instr) compileCmp]
           [(goto? instr) compileGoto]
           [(cjump? instr) compileCjump]
           [(print? instr) compilePrint]
           [(allocate? instr) compileAllocate]
           [(array-error? instr) compileArrayError])])
    (compfun instr)))

(define/contract (compileFun fn)
  (-> fun? string?)
  (string-append
   "("
   (string-join (map compileInstr (fun-instrs fn)) " ")
   ")"))

(provide compileFun)
