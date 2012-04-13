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
   (substring (symbol->string (label-name lab)) 1) ":"))

(define/contract (compileTarget targ)
  ((or/c symbol? integer?) . -> . string?)
  (cond
    [(symbol? targ) (symbol->string targ)]
    [(integer? targ) (string-replace (number->string targ) "$" 0 1)]))

(define/contract (compileReturn _)
  (return? . -> . string?)
  "ret\n")

(define/contract (compileCall kall)
  (call? . -> . string?)
  (string-append "call " (compileTarget (call-func kall))))

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
       ;; [`*=
       [`&= "andl "]
       [`<<= "sal "]
       [`>>= "sar "])
     (compileArg rarg)
     ", "
     (compileArg larg))))

(define/contract (compileCmp cmp)
  (cmp? . -> . string?)
  "#deadbeef")

(define/contract (compileGoto goto)
  (goto? . -> . string?)
  (string-append "jmp "
                 (compileTarget (goto-target goto))))

(define/contract (compileCjump cjmp)
  (cjump? . -> . string?)
  "#deadbeef")

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
       (assign (mem `esp -4) (car args))
       (assign (mem `esp 0) (cadr args))
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
