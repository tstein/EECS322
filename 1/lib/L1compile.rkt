#lang racket
(require srfi/13
         "L1types.rkt"
         "L1parse.rkt")

(define preamble
  ".text\n.globl _go\n.type _go, @function\n")

(define postamble
  ".size _go, .-_go\n.section .note.GNU-stack,\"\",@progbits\n")

(define/contract (regToLowReg reg)
  (symbol? . -> . symbol?)
  (string->symbol
   (string-append (substring (symbol->string reg) 1 2) "l")))

(define/contract (randomLabel)
  (-> string?)
  (string-append "_" (number->string (random 999999999))))

(define/contract (compileLabel lab)
  (label? . -> . string?)
  (string-append
   "_"
   (substring (symbol->string (label-name lab)) 1) ":"))

(define/contract (compileTarget targ)
  ((or/c symbol? integer?) . -> . string?)
  (cond
    [(symbol? targ) (if (member
                         targ
                         (list
                          `eax
                          `ebx
                          `ecx
                          `edx
                          `esi
                          `edi
                          `ebp
                          `esp))
                        (string-append "*%" (symbol->string targ))
                        (let ([targ (symbol->string targ)])
                          (if (string-prefix? ":" targ)
                              (string-append "_" (substring targ 1))
                              targ)))]
    [(integer? targ) (string-replace (number->string targ) "$" 0 1)]))

(define/contract (compileReturn _)
  (return? . -> . string?)
  "ret\n")

(define/contract (compileCall kall)
  (call? . -> . string?)
  (string-append "call " (compileTarget (call-func kall))))

(define (compileTailCall tcall)
  (string-append
   (compileInstr (call (tail-call-func tcall)))
   (compileInstr (return))))

(define/contract (compileArg arg)
  ((or/c integer? symbol? label? mem?) . -> . string?)
  (cond
    [(integer? arg) (string-append "$" (number->string arg))]
    [(symbol? arg) (let ([arg (symbol->string arg)])
                     (if (string-prefix? ":" arg)
                         (string-replace arg "$_" 0 1)
                         (string-append "%" arg)))]
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
                 (compileArg (assign-dst ass))))

(define/contract (compileMathop instr)
  (mathop? . -> . string?)
  (let ([op (mathop-op instr)]
        [larg (mathop-larg instr)]
        [rarg (mathop-rarg instr)])
    (string-append
     (match op
       [`+= "addl "]
       [`-= "subl "]
       [`*= "imul "]
       [`&= "andl "]
       [`<<= "sal "]
       [`>>= "sar "])
     (let ([rarg (if (and (or (eq? op `<<=)
                              (eq? op `>>=))
                          (symbol? rarg))
                     (regToLowReg rarg)
                     rarg)])
       (compileArg rarg))
     ", "
     (compileArg larg))))

(define/contract (compileCmp cmp)
  (cmp? . -> . string?)
  (let ([larg (cmp-larg cmp)]
        [rarg (cmp-rarg cmp)]
        [op (cmp-comparator cmp)]
        [opfun (match (cmp-comparator cmp)
                 [`< <]
                 [`<= <=]
                 [`= =])]
        [dest (cmp-destination cmp)])
    (if
     (and (integer? larg) (integer? rarg))
     (compileInstr (assign
                    dest
                    (if (opfun larg rarg)1 0)))
     (string-append
      (if (integer? larg)
          (string-append
           "cmpl "
           (compileArg larg)
           ", "
           (compileArg rarg)
           "\n")
          (string-append
           "cmpl "
           (compileArg rarg)
           ", "
           (compileArg larg)
           "\n"))
      (if (not (eq? null dest))
          (let ([ttarg (randomLabel)]
                [ftarg (randomLabel)]
                [aftarg (randomLabel)])
            (string-append
             "\t"
             (if (integer? larg)
                 (match op
                   [`<  "jg "]
                   [`<= "jge "]
                   [`=  "je "])
                 (match op
                   [`<  "jl "]
                   [`<= "jle "]
                   [`=  "je "]))
             ttarg "\n"
             "\tjmp " ftarg "\n"
             "\t" ttarg ":\n"
             (compileInstr (assign dest 1))
             "\tjmp " aftarg "\n"
             "\t" ftarg ":\n"
             (compileInstr (assign dest 0))
             "\t" aftarg ":\n"))
          "")))))

(define/contract (compileGoto goto)
  (goto? . -> . string?)
  (string-append "jmp _"
                 (substring (symbol->string (goto-target goto)) 1)))

(define/contract (compileCjump cjmp)
  (cjump? . -> . string?)
  (let ([larg (cjump-larg cjmp)]
        [rarg (cjump-rarg cjmp)]
        [op (cjump-op cjmp)]
        [ttarg (cjump-ttarget cjmp)]
        [ftarg (cjump-ftarget cjmp)])
    (if (and (integer? larg) (integer? rarg))
        (let ([opfun (match op
                       [`< <]
                       [`<= <=]
                       [`= =])])
          (if (opfun larg rarg)
              (compileGoto (goto ttarg))
              (compileGoto (goto ftarg))))
        (if (integer? larg)
            (string-append
             "cmpl "
             (compileArg larg)
             ", "
             (compileArg rarg)
             "\n\t"
             (match op
               [`<  "jg "]
               [`<= "jge "]
               [`=  "je "])
             (compileTarget ttarg)
             "\n\tjmp "
             (compileTarget ftarg))
            (string-append
             "cmpl "
             (compileArg rarg)
             ", "
             (compileArg larg)
             "\n\t"
             (match op
               [`<  "jl "]
               [`<= "jle "]
               [`=  "je "])
             (compileTarget ttarg)
             "\n\tjmp "
             (compileTarget ftarg))))))

(define/contract (runtime-instrs func args)
  (-> symbol? (listof (or/c integer? symbol?)) (listof l1instr?))
  (if (= (length args) 1)
      (list
       (mathop `-= `esp 4)
       (assign (mem `esp 0) (car args))
       (call func)
       (mathop `+= `esp 4))
      (list
       (mathop `-= `esp 8)
       (assign (mem `esp 0) (car args))
       (assign (mem `esp 4) (cadr args))
       (call func)
       (mathop `+= `esp 8))))

(define/contract (compilePrint prnt)
  (print? . -> . string?)
  (foldr
   string-append
   ""
   (map compileInstr (runtime-instrs `print (list (print-val prnt))))))

(define/contract (compileAllocate alloc)
  (allocate? . -> . string?)
  (foldr
   string-append
   ""
   (map compileInstr (runtime-instrs
                      `allocate
                      (list (allocate-size alloc)
                            (allocate-init alloc))))))

(define/contract (compileArrayError arrerr)
  (array-error? . -> . string?)
  (foldr
   string-append
   ""
   (map compileInstr (runtime-instrs
                      `print_error
                      (list (array-error-ptr arrerr)
                            (array-error-index arrerr))))))

(define/contract (compileInstr instr)
  (l1instr? . -> . string?)
  (string-append
   "\t"
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
   (foldr string-append "" (cons
                            (string-append (compileLabel (l1fun-name fun)) "\n")
                            (map compileInstr (l1fun-instrs fun))))
   (if (eq? (label-name (l1fun-name fun)) `:go)
       "\tmovl $0, %eax\n\tleave\n\tret\n"
       ""
       )))

(define/contract (compile prog)
  (l1prog? . -> . string?)
  (string-append
   preamble
   (foldr string-append "" (map compileFun (l1prog-funs prog)))
   postamble))

(provide compile)
