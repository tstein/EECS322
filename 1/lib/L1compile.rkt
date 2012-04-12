#lang racket
(require srfi/13
         "L1types.rkt"
         "L1parse.rkt")

(define preamble
  ".text\n.globl go\n.type go, @function\n")

(define postamble
  ".size go, .-go\n.section .note.GNU-stack,\"\",@progbits\n")

(define/contract (compileLabel lab)
  (label? . -> . string?)
  (string-append 
   (substring (symbol->string (label-name lab)) 1) ":\n"))

(define/contract (compileTarget sym)
  (symbol? . -> . string?)
  (string-replace (symbol->string (label-name sym) "$" 0 1)))

(define/contract (compileReturn _)
  (return? . -> . string?)
  "ret\n")

(define/contract (compileCall kall)
  (call? . -> . string?)
  (string-append "call " (call-func kall) "\n"))

;; FIXME
(define compileTailCall compileCall)

(define/contract (compileArg arg)
  ((or/c integer? symbol? label? mem?) . -> . string?)
  (cond
    [(integer? arg) (string-append "$" (number->string arg))]
    [(symbol? arg) (string-append "%" (symbol->string arg))]
    [(label? arg) (compileTarget (label-name arg))]
    [(mem? arg) (string-append
                 (number->string (mem-offset arg))
                 "("
                 (compileArg (mem-addr arg))
                 ")")]))

(define/contract (compileAssign ass)
  (assign? . -> . string?)
  (string-append "movl "
                 (compileArg (assign-src ass))
                 ", "
                 (compileArg (assign-dst ass))
                 "\n"))

(define/contract (compileMathop instr)
  (mathop? . -> . string?)
  (let ([op (mathop-op instr)]
        [larg (mathop-larg instr)]
        [rarg (mathop-rarg instr)])
    (string-append
     (match op
       [`+= "addl "]
       [`-= "subl "]
       ;; [`*=
       [`&= "andl "]
       [`<<= "sal "]
       [`>>= "sar "])
     (compileArg rarg)
     " "
     (compileArg larg)
     "\n")))

(define/contract (compileCmp cmp)
  (cmp? . -> . string?)
  "deadbeef")

(define/contract (compileGoto goto)
  (goto? . -> . string?)
  (string-append "jmp "
                 (compileTarget (goto-target goto))
                 "\n"))

(define/contract (compileCjump cjmp)
  (cjump? . -> . string?)
  "deadbeef")

(define/contract (compilePrint prnt)
  (print? . -> . string?)
  "deadbeef")

(define/contract (compileAllocate alloc)
  (allocate? . -> . string?)
  "deadbeef")

(define/contract (compileArrayError arrerr)
  (array-error? . -> . string?)
  "deadbeef")

(define/contract (compileInstr instr)
  (l1instr? . -> . string?)
  (string-append
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
     (compfun instr))
   "\n"))

(define/contract (compileFun fun)
  (l1fun? . -> . string?)
  (string-append
   (foldl string-append "" (cons (compileLabel (l1fun-name fun))
                                 (map compileInstr (l1fun-instrs fun))))
   (if (eq? (label-name (l1fun-name fun)) `:go)
       "movl $0, %eax\nret"
       ""
       )))

(define/contract (compile prog)
  (l1prog? . -> . string?)
  (string-append
   preamble
   (foldl string-append "" (map compileFun (l1prog-funs prog)))
   postamble))
