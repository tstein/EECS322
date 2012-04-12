#lang racket
(require srfi/13
         "L1types.rkt"
         "L1parse.rkt")

(define preamble
  ".text\n.globl go\n.type go, @function\nn")

(define postamble
  ".size go, .-go\n.section .note.GNU-stack,\"\",@progbits\n")

(define (compileLabel lab)
  (string-append 
   (substring (symbol->string (label-name lab)) 1) ":\n"))

(define (compileTarget sym)
  (string-replace (symbol->string (label-name sym) "$" 0 1)))

(define (compileReturn _)
  "ret\n")

(define (compileCall kall)
  (string-append "call " (call-func kall) "\n"))

;; FIXME
(define compileTailCall compileCall)

(define (compileArg arg)
  (cond
    [(integer? arg) (string-append "$" (number->string arg))]
    [(symbol? arg) (string-append "%" (symbol->string arg))]
    [(label? arg) (compileTarget (label-name arg))]
    [(mem? arg) (string-append
                 (number->string (mem-offset arg))
                 "("
                 (compileArg (mem-addr arg))
                 ")")]))

(define (compileAssign ass)
  (string-append "movl "
                 (compileArg (assign-src ass))
                 ", "
                 (compileArg (assign-dst ass))
                 "\n"))

(define (compileMathop instr)
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

(define (compileCmp cmp) #f)

(define (compileGoto goto)
  (string-append "jmp "
                 (compileTarget (goto-target goto))
                 "\n"))

(define (compileCjump cjmp) #f)

(define (compilePrint prnt) #f)

(define (compileAllocate prnt) #f)

(define (compileArrayError prnt) #f)

(define (compileInstr instr)
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

(define (compileFun fun)
  (foldl string-append "" (cons (compileLabel (l1fun-name fun))
                                (map compileInstr (l1fun-instrs fun)))))

(define (compile prog)
  (string-append
   preamble
   (foldl string-append "" (map compileFun (l1prog-funs prog)))
   postamble))
