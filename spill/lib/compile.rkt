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
  mthp)

(define/contract (compileCmp cmp)
  (cmp? . -> . string?)
  cmp)

(define/contract (compileGoto goto)
  (goto? . -> . string?)
  goto)

(define/contract (compileCjump cjmp)
  (cjump? . -> . string?)
  cjmp)

(define/contract (compilePrint prnt)
  (print? . -> . string?)
  prnt)

(define/contract (compileAllocate alloc)
  (allocate? . -> . string?)
  alloc)

(define/contract (compileArrayError arrerr)
  (array-error? . -> . string?)
  arrerr)

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
